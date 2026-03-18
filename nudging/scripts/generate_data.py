#!/usr/bin/env python3
# Copyright (c) 2025
# Manuel Cherep <mcherep@mit.edu>
# Nikhil Singh <nsingh1@mit.edu>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Neighborhoods replace baskets; housing features replace prizes. Feature scores
come from the actual Boston Housing dataset, normalized to [0, 10].
Features that are lower-is-better are flipped so that higher score always means
more desirable.

    A: Rooms            (rm)       — higher is better
    B: Safety           (crim)     — lower is better → flipped
    C: Job Proximity    (dis)      — lower is better → flipped
    D: School Quality   (ptratio)  — lower is better → flipped
    E: Low Tax Rate     (tax)      — lower is better → flipped

When a trial uses fewer than 5 features, the first k features are used (e.g.
2-feature trials show Rooms and Safety; 3-feature trials add Job Proximity).

Weights follow Callaway et al.: positive integers summing to 30, each at least 1.
"""

import argparse
import os

import numpy as np
import pandas as pd
import statsmodels.api as sm

FEATURES = ["rm", "crim", "dis", "ptratio", "tax"]
FLIP_FEATURES = {"crim", "dis", "ptratio", "tax"}

N_FEATURES = 5       # maximum number of features
N_NEIGHBORHOODS = 5  # default number of neighborhoods


# ---------------------------------------------------------------------------
# Data loading & normalisation
# ---------------------------------------------------------------------------


def normalize_boston(df: pd.DataFrame) -> pd.DataFrame:
    """Select the 5 features and normalize each to [0, 10]."""
    result = df[FEATURES].copy()
    for feat in FEATURES:
        lo, hi = result[feat].min(), result[feat].max()
        norm = (result[feat] - lo) / (hi - lo) * 10
        if feat in FLIP_FEATURES:
            norm = 10 - norm
        result[feat] = norm.round(2)
    return result.reset_index(drop=True)


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

def sample_weights(rng: np.random.Generator, n_features: int) -> list[int]:
    """
    Sample n_features positive integers that sum to 30.
    Follows Callaway et al.: prize values randomly sampled with the constraint
    that they sum to 30 points and each prize is worth at least one point.
    Uses a stars-and-bars approach.
    """
    cuts = np.sort(rng.choice(np.arange(1, 30), size=n_features - 1, replace=False))
    return [int(w) for w in np.diff(np.concatenate([[0], cuts, [30]]))]


def sample_highlight_weights(
    rng: np.random.Generator,
    highlight_index: int,
    highlight_weight: int,
    n_features: int,
) -> list[int]:
    """
    Sample weights for all features given a fixed weight for the highlighted
    feature. The remaining (n_features - 1) weights are positive integers
    summing to (30 - highlight_weight), each at least 1.
    """
    remaining = 30 - highlight_weight
    n_remaining = n_features - 1
    cuts = np.sort(rng.choice(np.arange(1, remaining), size=n_remaining - 1, replace=False))
    other_weights = [int(w) for w in np.diff(np.concatenate([[0], cuts, [remaining]]))]
    return other_weights[:highlight_index] + [highlight_weight] + other_weights[highlight_index:]


def default_neighborhood_index(payoff_matrix: np.ndarray, rng: np.random.Generator) -> int:
    """
    Return the index of the default neighborhood: the one with the highest
    unweighted sum of feature scores (Callaway et al., Equation 10).
    Ties are broken randomly, as in the paper.
    """
    col_sums = payoff_matrix.sum(axis=0)
    return int(rng.choice(np.flatnonzero(col_sums == col_sums.max())))


def shuffled_cycle(configs: list, n: int, rng: np.random.Generator) -> list:
    """
    Return a list of length n by tiling configs in shuffled blocks.
    Ensures each config appears as equally often as possible.
    """
    result = []
    while len(result) < n:
        block = configs.copy()
        rng.shuffle(block)
        result.extend(block)
    return result[:n]


# ---------------------------------------------------------------------------
# Per-nudge generators
# ---------------------------------------------------------------------------

def generate_default(
    boston_norm: pd.DataFrame,
    n_trials: int,
    cost: int,
    seed: int,
) -> pd.DataFrame:
    """
    Generate trials for the Default nudge.

    Balances 8 trial types:
      {control, default} × {2, 5} baskets × {2, 5} features
    Trial order is shuffled.
    """
    rng = np.random.default_rng(seed)

    base_configs = [
        (trial_nudge, n_b, n_f)
        for trial_nudge in ["control", "default"]
        for n_b in [2, 5]
        for n_f in [2, 5]
    ]

    rows = []
    configs = shuffled_cycle(base_configs, n_trials, rng)
    for trial_num, (trial_nudge, n_b, n_f) in enumerate(configs, start=1):
        neighborhood_idxs = rng.choice(len(boston_norm), size=n_b, replace=False)
        # First n_f features (columns) of the normalized dataset
        payoff_matrix = boston_norm.iloc[neighborhood_idxs, :n_f].values.T
        weights = sample_weights(rng, n_f)
        nudge_index = (
            default_neighborhood_index(payoff_matrix, rng)
            if trial_nudge == "default"
            else 0
        )
        rows.append({
            "is_practice": False,
            "payoff_matrix": str(payoff_matrix.tolist()),
            "weights": str(weights),
            "trial_nudge": trial_nudge,
            "nudge_index": nudge_index,
            "cost": cost,
            "uncovered_values": "[]",
            "accepted_default": False,
            "trial_num": trial_num,
            "participant_id": 1,
            "nudge_type": "default",
        })
    return pd.DataFrame(rows)


def generate_suggestion(
    boston_norm: pd.DataFrame,
    n_trials: int,
    cost: int,
    seed: int,
) -> pd.DataFrame:
    """
    Generate trials for the Suggestion (supersize) nudge.

    Balances 6 trial types:
      {control, pre-supersize, post-supersize} × {2, 5} features
    Trial order is shuffled.

    Per Callaway et al. Experiment 2:
    - Nudge trials have 6 neighborhoods; control trials have 5.
    - pre-supersize:  suggested neighborhood chosen randomly from the 6;
                      its highest feature score is revealed upfront.
    - post-supersize: the 6th (rightmost) neighborhood introduced after the
                      agent's first choice among the initial 5.
    """
    rng = np.random.default_rng(seed)

    base_configs = [
        (trial_nudge, n_f)
        for trial_nudge in ["control", "pre-supersize", "post-supersize"]
        for n_f in [2, 5]
    ]

    n_nudge_neighborhoods = N_NEIGHBORHOODS + 1  # 6 for nudge trials

    rows = []
    configs = shuffled_cycle(base_configs, n_trials, rng)
    for trial_num, (trial_nudge, n_f) in enumerate(configs, start=1):
        weights = sample_weights(rng, n_f)

        if trial_nudge == "post-supersize":
            idxs = rng.choice(len(boston_norm), size=n_nudge_neighborhoods, replace=False)
            payoff_matrix = boston_norm.iloc[idxs, :n_f].values.T
            og_baskets = N_NEIGHBORHOODS
            shown_baskets = n_nudge_neighborhoods
            nudge_index = n_nudge_neighborhoods - 1  # rightmost
        elif trial_nudge == "pre-supersize":
            idxs = rng.choice(len(boston_norm), size=n_nudge_neighborhoods, replace=False)
            payoff_matrix = boston_norm.iloc[idxs, :n_f].values.T
            og_baskets = n_nudge_neighborhoods
            shown_baskets = n_nudge_neighborhoods
            nudge_index = int(rng.integers(n_nudge_neighborhoods))  # random
        else:  # control
            idxs = rng.choice(len(boston_norm), size=N_NEIGHBORHOODS, replace=False)
            payoff_matrix = boston_norm.iloc[idxs, :n_f].values.T
            og_baskets = N_NEIGHBORHOODS
            shown_baskets = N_NEIGHBORHOODS
            nudge_index = -1

        rows.append({
            "is_practice": False,
            "payoff_matrix": str(payoff_matrix.tolist()),
            "weights": str(weights),
            "trial_nudge": trial_nudge,
            "nudge_index": nudge_index,
            "og_baskets": og_baskets,
            "shown_baskets": shown_baskets,
            "cost": cost,
            "uncovered_values": "[]",
            "selected_option": 0,
            "trial_num": trial_num,
            "participant_id": 1,
            "nudge_type": "suggestion",
        })
    return pd.DataFrame(rows)


def generate_highlight(
    boston_norm: pd.DataFrame,
    n_trials: int,
    normal_cost: int,
    highlight_cost: int,
    seed: int,
) -> pd.DataFrame:
    """
    Generate trials for the Highlight nudge.

    Follows Callaway et al. Experiment 3 exactly:
    - Always 5 neighborhoods and 3 features (first 3: Rooms, Safety, Job Proximity).
    - Randomly decide whether nudge trials draw the highlighted feature's weight
      from even integers [2, 28] or odd integers [1, 27].
    - Nudge trials: draw without replacement from that pool.
    - Control trials: draw without replacement from the complementary pool.
    - Non-highlighted features: positive integers summing to (30 - highlight_weight),
      each at least 1.
    - If the pool is exhausted (more trials than pool size), it is reshuffled and reused.
    - On every trial one feature is randomly selected as the highlighted feature.
      On nudge trials its cost is reduced; on control trials cost is unchanged.
    """
    N_HIGHLIGHT_FEATURES = 3
    N_HIGHLIGHT_NEIGHBORHOODS = 5

    # Pools exactly as in the paper (feasible with 3 features: max weight = 28)
    even_pool = np.arange(2, 29, 2)  # 2, 4, ..., 28  (14 values)
    odd_pool  = np.arange(1, 28, 2)  # 1, 3, ..., 27  (14 values)

    rng = np.random.default_rng(seed)
    use_even_for_nudge = bool(rng.integers(2))
    nudge_pool   = list(rng.permutation(even_pool if use_even_for_nudge else odd_pool))
    control_pool = list(rng.permutation(odd_pool  if use_even_for_nudge else even_pool))

    rows = []
    for trial_num in range(1, n_trials + 1):
        is_control = trial_num % 2 == 0

        idxs = rng.choice(len(boston_norm), size=N_HIGHLIGHT_NEIGHBORHOODS, replace=False)
        payoff_matrix = boston_norm.iloc[idxs, :N_HIGHLIGHT_FEATURES].values.T

        highlight_index = int(rng.integers(N_HIGHLIGHT_FEATURES))

        pool = control_pool if is_control else nudge_pool
        if not pool:
            refill = (odd_pool if use_even_for_nudge else even_pool) if is_control \
                else (even_pool if use_even_for_nudge else odd_pool)
            pool[:] = list(rng.permutation(refill))
        highlight_weight = int(pool.pop(0))

        weights = sample_highlight_weights(rng, highlight_index, highlight_weight, N_HIGHLIGHT_FEATURES)

        cost_matrix = np.full((N_HIGHLIGHT_FEATURES, N_HIGHLIGHT_NEIGHBORHOODS), normal_cost, dtype=int)
        if not is_control:
            cost_matrix[highlight_index, :] = highlight_cost

        rows.append({
            "is_practice": False,
            "payoff_matrix": str(payoff_matrix.tolist()),
            "weights": str(weights),
            "original_cost_matrix": str(cost_matrix.tolist()),
            "highlight_index": highlight_index,
            "is_control": is_control,
            "uncovered_values": "[]",
            "selected_option": 0,
            "trial_num": trial_num,
            "trial_type": "control" if is_control else "highlight",
            "participant_id": 1,
        })
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

GENERATORS = {
    "default": lambda boston_norm, args: generate_default(
        boston_norm, args.n_trials, args.cost, args.seed
    ),
    "suggestion": lambda boston_norm, args: generate_suggestion(
        boston_norm, args.n_trials, args.cost, args.seed
    ),
    "highlight": lambda boston_norm, args: generate_highlight(
        boston_norm, args.n_trials,
        args.normal_cost, args.highlight_cost, args.seed
    ),
}


def main():
    parser = argparse.ArgumentParser(
        description="Generate Boston Housing nudging data for a single nudge type"
    )
    parser.add_argument(
        "nudge", choices=GENERATORS.keys(),
        help="Which nudge type to generate",
    )
    parser.add_argument(
        "--output", required=True,
        help="Path to the output CSV file",
    )
    parser.add_argument("--n-trials", type=int, required=True)
    parser.add_argument(
        "--cost", type=int, default=2,
        help="Cost per reveal (default and suggestion nudges)",
    )
    parser.add_argument(
        "--normal-cost", type=int, default=3,
        help="Normal cost per reveal (highlight nudge)",
    )
    parser.add_argument(
        "--highlight-cost", type=int, default=1,
        help="Discounted cost for the highlighted feature (highlight nudge)",
    )
    parser.add_argument("--seed", type=int, default=42)
    args = parser.parse_args()

    print("Loading Boston Housing data...")
    boston_norm = normalize_boston(sm.datasets.get_rdataset("Boston", "MASS").data)
    print(f"  {len(boston_norm)} neighborhoods, {N_FEATURES} features")

    print(f"Generating {args.nudge} data...")
    df = GENERATORS[args.nudge](boston_norm, args)
    os.makedirs(os.path.dirname(os.path.abspath(args.output)), exist_ok=True)
    df.to_csv(args.output, index=False)
    print(f"  {len(df)} rows written to {args.output}")


if __name__ == "__main__":
    main()
