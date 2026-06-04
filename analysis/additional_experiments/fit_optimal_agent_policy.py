import os
import math
import argparse
import re
import ast
import json
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input_path",
        type=str,
        default="data/data-optimal.csv"
    )
    parser.add_argument(
        "--output_path",
        type=str,
        default=""
    )
    parser.add_argument(
        "--source",
        nargs="+",
        default=["all_agents"]
    )
    parser.add_argument(
        "--nudge_type",
        nargs="*",
        default=[]
    )
    parser.add_argument(
        "--prior_mean",
        type=float,
        default=5.0
    )
    parser.add_argument(
        "--prior_sigma",
        type=float,
        default=1.75
    )
    parser.add_argument(
        "--max_iter",
        type=int,
        default=2000
    )
    args = parser.parse_args()
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_path = resolve_path(
        path=args.input_path,
        base_dir=script_dir
    )

    df, resolved_sources = load_trials(
        input_path=input_path,
        sources=args.source,
        nudge_types=args.nudge_type
    )
    policy_name = slugify_sources(resolved_sources)
    output_path = args.output_path
    if output_path == "":
        output_path = os.path.join(
            script_dir,
            "..",
            "..",
            "optimal-nudging",
            "model",
            "results",
            "agent_policies",
            "%s.json" % (policy_name)
        )
    else:
        output_path = resolve_path(
            path=output_path,
            base_dir=os.getcwd()
        )

    datasets = build_training_sets(
        df=df,
        prior_mean=args.prior_mean,
        prior_sigma=args.prior_sigma
    )

    policy = {
        "policy_name": policy_name,
        "sources": resolved_sources,
        "prior_mean": args.prior_mean,
        "prior_sigma": args.prior_sigma,
        "n_options": int(df["n_baskets"].iloc[0]),
        "n_features": int(df["n_prizes"].iloc[0]),
        "training_summary": {
            "n_trials": int(df.shape[0]),
            "n_stop_states": int(len(datasets["stop_labels"])),
            "n_reveal_states": int(sum(datasets["reveal_group_sizes"])),
            "n_choice_states": int(sum(datasets["choice_group_sizes"])),
            "n_invalid_reveals_skipped": int(datasets["invalid_reveals_skipped"]),
            "trial_counts_by_source": df["source"].value_counts().sort_index().to_dict(),
            "trial_counts_by_nudge_type": df["nudge_type"].value_counts().sort_index().to_dict()
        },
        "models": {
            "stop": fit_binary_model(
                features=datasets["stop_features"],
                labels=datasets["stop_labels"],
                feature_names=STOP_FEATURE_NAMES,
                max_iter=args.max_iter
            ),
            "reveal": fit_binary_model(
                features=datasets["reveal_features"],
                labels=datasets["reveal_labels"],
                feature_names=REVEAL_FEATURE_NAMES,
                max_iter=args.max_iter
            ),
            "choice": fit_binary_model(
                features=datasets["choice_features"],
                labels=datasets["choice_labels"],
                feature_names=CHOICE_FEATURE_NAMES,
                max_iter=args.max_iter
            )
        }
    }

    output_dir = os.path.dirname(output_path)
    if output_dir != "":
        os.makedirs(output_dir, exist_ok=True)

    with open(output_path, "w") as outfile:
        json.dump(policy, outfile, indent=2)

    print("Wrote fitted policy to %s" % (output_path))
    print(
        "Training rows: stop=%s reveal=%s choice=%s invalid_reveals_skipped=%s" % (
            len(datasets["stop_labels"]),
            len(datasets["reveal_labels"]),
            len(datasets["choice_labels"]),
            datasets["invalid_reveals_skipped"]
        )
    )


STOP_FEATURE_NAMES = [
    "best_value",
    "best_gap",
    "n_revealed",
    "frac_revealed",
    "max_voc",
    "mean_voc",
    "positive_voc_count",
    "min_available_cost",
    "mean_available_cost",
    "best_option_observed_fraction",
    "idiosyncrasy"
]

REVEAL_FEATURE_NAMES = [
    "voc",
    "voi",
    "cell_cost",
    "weight",
    "weight_dev",
    "option_value",
    "option_gap_from_best",
    "observed_fraction_option",
    "option_uncertainty",
    "is_in_best_option"
]

CHOICE_FEATURE_NAMES = [
    "option_value",
    "option_gap_from_best",
    "observed_fraction_option",
    "option_uncertainty",
    "is_current_best"
]

DEFAULT_TRAIN_NUDGE_TYPES = [
    "random",
    "extreme"
]


def load_trials(
    input_path: str,
    sources: list[str],
    nudge_types: list[str]
) -> tuple[pd.DataFrame, list[str]]:
    df = pd.read_csv(input_path)
    df = df[df["nudge"] == "optimal"].copy()
    df = df[~df["is_practice"].map(as_bool)].copy()

    if len(sources) == 1 and sources[0] == "all_agents":
        df = df[df["source"] != "real"].copy()
        resolved_sources = sorted(df["source"].unique().tolist())
    else:
        df = df[df["source"].isin(sources)].copy()
        resolved_sources = list(sources)

    if len(nudge_types) == 0:
        nudge_types = DEFAULT_TRAIN_NUDGE_TYPES

    df = df[df["nudge_type"].isin(nudge_types)].copy()

    assert df.shape[0] > 0, "No optimal trials matched the requested filters"
    assert df["n_baskets"].nunique() == 1
    assert df["n_prizes"].nunique() == 1

    return df.reset_index(drop=True), resolved_sources


def build_training_sets(
    df: pd.DataFrame,
    prior_mean: float,
    prior_sigma: float
) -> dict[str, list]:
    stop_features = []
    stop_labels = []
    reveal_features = []
    reveal_labels = []
    reveal_group_sizes = []
    choice_features = []
    choice_labels = []
    choice_group_sizes = []
    invalid_reveals_skipped = 0

    for row in df.itertuples():
        payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix), dtype=float)
        cost_matrix = np.array(ast.literal_eval(row.cost_matrix), dtype=float)
        weights = np.array(ast.literal_eval(row.weights), dtype=float)
        revealed = (cost_matrix == 0)
        uncovered_values = list(ast.literal_eval(row.uncovered_values))
        selected_option = int(row.selected_option)

        for flat_index in uncovered_values:
            state_values = compute_state_features(
                payoff_matrix=payoff_matrix,
                cost_matrix=cost_matrix,
                weights=weights,
                revealed=revealed,
                prior_mean=prior_mean,
                prior_sigma=prior_sigma
            )

            row_index = flat_index // payoff_matrix.shape[1]
            col_index = flat_index % payoff_matrix.shape[1]
            if revealed[row_index, col_index]:
                stop_features.append(state_values)
                stop_labels.append(0)
                invalid_reveals_skipped += 1
                continue

            stop_features.append(state_values)
            stop_labels.append(0)

            available = available_cells(revealed=revealed)
            reveal_group_sizes.append(len(available))
            for cell_index in available:
                reveal_features.append(
                    compute_cell_features(
                        payoff_matrix=payoff_matrix,
                        cost_matrix=cost_matrix,
                        weights=weights,
                        revealed=revealed,
                        prior_mean=prior_mean,
                        prior_sigma=prior_sigma,
                        flat_index=cell_index
                    )
                )
                reveal_labels.append(int(cell_index == flat_index))

            revealed[row_index, col_index] = True

        stop_features.append(
            compute_state_features(
                payoff_matrix=payoff_matrix,
                cost_matrix=cost_matrix,
                weights=weights,
                revealed=revealed,
                prior_mean=prior_mean,
                prior_sigma=prior_sigma
            )
        )
        stop_labels.append(1)

        available_options = list(range(payoff_matrix.shape[1]))
        choice_group_sizes.append(len(available_options))
        for option_index in available_options:
            choice_features.append(
                compute_choice_features(
                    payoff_matrix=payoff_matrix,
                    weights=weights,
                    revealed=revealed,
                    prior_mean=prior_mean,
                    prior_sigma=prior_sigma,
                    option_index=option_index
                )
            )
            choice_labels.append(int(option_index == selected_option))

    return {
        "stop_features": stop_features,
        "stop_labels": stop_labels,
        "reveal_features": reveal_features,
        "reveal_labels": reveal_labels,
        "reveal_group_sizes": reveal_group_sizes,
        "choice_features": choice_features,
        "choice_labels": choice_labels,
        "choice_group_sizes": choice_group_sizes,
        "invalid_reveals_skipped": invalid_reveals_skipped
    }


def fit_binary_model(
    features: list[list[float]],
    labels: list[int],
    feature_names: list[str],
    max_iter: int
) -> dict[str, object]:
    x_values = np.asarray(features, dtype=float)
    y_values = np.asarray(labels, dtype=int)

    assert x_values.shape[0] == y_values.shape[0]
    assert x_values.shape[1] == len(feature_names)
    assert np.unique(y_values).size == 2

    scaler = StandardScaler()
    x_scaled = scaler.fit_transform(x_values)
    scale = scaler.scale_.copy()
    scale[scale == 0] = 1.0

    model = LogisticRegression(
        max_iter=max_iter,
        class_weight="balanced",
        solver="lbfgs"
    )
    model.fit(x_scaled, y_values)

    predictions = model.predict(x_scaled)
    probabilities = model.predict_proba(x_scaled)[:, 1]

    return {
        "feature_names": feature_names,
        "coef": model.coef_[0].astype(float).tolist(),
        "intercept": float(model.intercept_[0]),
        "mean": scaler.mean_.astype(float).tolist(),
        "scale": scale.astype(float).tolist(),
        "positive_rate": float(np.mean(y_values)),
        "train_accuracy": float(np.mean(predictions == y_values)),
        "train_log_loss": float(binary_log_loss(y_values, probabilities))
    }


def compute_state_features(
    payoff_matrix: np.ndarray,
    cost_matrix: np.ndarray,
    weights: np.ndarray,
    revealed: np.ndarray,
    prior_mean: float,
    prior_sigma: float
) -> list[float]:
    option_values, option_uncertainty, option_observed_fraction = compute_option_stats(
        payoff_matrix=payoff_matrix,
        weights=weights,
        revealed=revealed,
        prior_mean=prior_mean,
        prior_sigma=prior_sigma
    )
    available = available_cells(revealed=revealed)
    cell_vocs = [
        compute_cell_features(
            payoff_matrix=payoff_matrix,
            cost_matrix=cost_matrix,
            weights=weights,
            revealed=revealed,
            prior_mean=prior_mean,
            prior_sigma=prior_sigma,
            flat_index=cell_index
        )[0]
        for cell_index in available
    ]

    best_option = int(np.argmax(option_values))
    best_value = float(option_values[best_option])
    second_value = best_value
    if option_values.shape[0] > 1:
        second_value = float(np.partition(option_values, -2)[-2])

    costs = [float(cost_matrix.flat[cell_index]) for cell_index in available]
    idiosyncrasy = float(np.sum(np.abs(weights - np.mean(weights))))

    return [
        best_value,
        best_value - second_value,
        float(np.sum(revealed)),
        float(np.mean(revealed)),
        max(cell_vocs) if len(cell_vocs) > 0 else 0.0,
        float(np.mean(cell_vocs)) if len(cell_vocs) > 0 else 0.0,
        float(sum(voc > 0 for voc in cell_vocs)),
        min(costs) if len(costs) > 0 else 0.0,
        float(np.mean(costs)) if len(costs) > 0 else 0.0,
        float(option_observed_fraction[best_option]),
        idiosyncrasy
    ]


def compute_cell_features(
    payoff_matrix: np.ndarray,
    cost_matrix: np.ndarray,
    weights: np.ndarray,
    revealed: np.ndarray,
    prior_mean: float,
    prior_sigma: float,
    flat_index: int
) -> list[float]:
    n_options = payoff_matrix.shape[1]
    row_index = flat_index // n_options
    col_index = flat_index % n_options
    assert not revealed[row_index, col_index]

    option_values, option_uncertainty, option_observed_fraction = compute_option_stats(
        payoff_matrix=payoff_matrix,
        weights=weights,
        revealed=revealed,
        prior_mean=prior_mean,
        prior_sigma=prior_sigma
    )
    best_option = int(np.argmax(option_values))
    best_value = float(option_values[best_option])
    competing_values = np.delete(option_values, col_index)
    competing_value = float(np.max(competing_values)) if competing_values.size > 0 else best_value

    weight = float(weights[row_index])
    target_mean = weight * prior_mean
    target_sigma = weight * prior_sigma
    revealed_sum = option_values[col_index] - target_mean
    option_if_revealed_mean = revealed_sum + target_mean
    voi = normal_emax(
        mean=option_if_revealed_mean,
        sigma=target_sigma,
        constant=competing_value
    ) - best_value
    option_gap = float(option_values[col_index] - best_value)

    return [
        voi - float(cost_matrix[row_index, col_index]),
        voi,
        float(cost_matrix[row_index, col_index]),
        weight,
        float(weight - np.mean(weights)),
        float(option_values[col_index]),
        option_gap,
        float(option_observed_fraction[col_index]),
        float(option_uncertainty[col_index]),
        float(col_index == best_option)
    ]


def compute_choice_features(
    payoff_matrix: np.ndarray,
    weights: np.ndarray,
    revealed: np.ndarray,
    prior_mean: float,
    prior_sigma: float,
    option_index: int
) -> list[float]:
    option_values, option_uncertainty, option_observed_fraction = compute_option_stats(
        payoff_matrix=payoff_matrix,
        weights=weights,
        revealed=revealed,
        prior_mean=prior_mean,
        prior_sigma=prior_sigma
    )
    best_option = int(np.argmax(option_values))
    best_value = float(option_values[best_option])

    return [
        float(option_values[option_index]),
        float(option_values[option_index] - best_value),
        float(option_observed_fraction[option_index]),
        float(option_uncertainty[option_index]),
        float(option_index == best_option)
    ]


def compute_option_stats(
    payoff_matrix: np.ndarray,
    weights: np.ndarray,
    revealed: np.ndarray,
    prior_mean: float,
    prior_sigma: float
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    n_features, n_options = payoff_matrix.shape
    option_values = np.zeros(n_options, dtype=float)
    option_uncertainty = np.zeros(n_options, dtype=float)
    option_observed_fraction = np.zeros(n_options, dtype=float)

    for option_index in range(n_options):
        variance_sum = 0.0
        observed_count = 0
        for feature_index in range(n_features):
            weight = float(weights[feature_index])
            if revealed[feature_index, option_index]:
                option_values[option_index] += weight * float(payoff_matrix[feature_index, option_index])
                observed_count += 1
            else:
                option_values[option_index] += weight * prior_mean
                variance_sum += (weight * prior_sigma) ** 2
        option_uncertainty[option_index] = math.sqrt(variance_sum)
        option_observed_fraction[option_index] = observed_count / float(n_features)

    return option_values, option_uncertainty, option_observed_fraction


def available_cells(
    revealed: np.ndarray
) -> list[int]:
    n_features, n_options = revealed.shape
    return [
        (feature_index * n_options) + option_index
        for feature_index in range(n_features)
        for option_index in range(n_options)
        if not revealed[feature_index, option_index]
    ]


def normal_emax(
    mean: float,
    sigma: float,
    constant: float
) -> float:
    if sigma < 1e-8:
        return max(mean, constant)

    z_value = (constant - mean) / sigma
    cdf_value = normal_cdf(z_value)
    pdf_value = normal_pdf(z_value)
    return (constant * cdf_value) + (mean * (1.0 - cdf_value)) + (sigma * pdf_value)


def normal_cdf(
    value: float
) -> float:
    return 0.5 * (1.0 + math.erf(value / math.sqrt(2.0)))


def normal_pdf(
    value: float
) -> float:
    return math.exp(-0.5 * (value ** 2)) / math.sqrt(2.0 * math.pi)


def binary_log_loss(
    labels: np.ndarray,
    probabilities: np.ndarray
) -> float:
    clipped = np.clip(probabilities, 1e-8, 1.0 - 1e-8)
    return float(
        -np.mean(
            (labels * np.log(clipped)) + ((1 - labels) * np.log(1.0 - clipped))
        )
    )


def as_bool(
    value: object
) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        return bool(value)
    return str(value).lower() == "true"


def slugify_sources(
    sources: list[str]
) -> str:
    if len(sources) == 0:
        return "empty"
    if len(sources) > 4:
        return "%s_models" % (len(sources))

    cleaned = [
        re.sub(r"[^a-z0-9]+", "-", source.lower()).strip("-")
        for source in sources
    ]
    return "-".join(cleaned)


def resolve_path(
    path: str,
    base_dir: str
) -> str:
    if os.path.isabs(path):
        return path
    return os.path.normpath(os.path.join(base_dir, path))


if __name__ == "__main__":
    main()
