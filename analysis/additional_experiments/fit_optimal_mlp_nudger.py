import os
import random
import argparse
import ast
import json
import pickle
import numpy as np
import pandas as pd
from sklearn.compose import TransformedTargetRegressor
from sklearn.metrics import mean_absolute_error, r2_score
from sklearn.linear_model import Ridge
from sklearn.neural_network import MLPRegressor
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--results_path",
        type=str,
        default="data/data-optimal-prev.csv"
    )
    parser.add_argument(
        "--template_path",
        type=str,
        default="../nudging/data/optimal_nudging_changing_belief_state_data.csv"
    )
    parser.add_argument(
        "--source",
        type=str,
        required=True
    )
    parser.add_argument(
        "--train_nudge_type",
        nargs="*",
        default=["random", "extreme"]
    )
    parser.add_argument(
        "--scaffold_nudge_type",
        type=str,
        default="greedy"
    )
    parser.add_argument(
        "--output_nudge_type",
        type=str,
        default="optimal_ai_mlp"
    )
    parser.add_argument(
        "--reward_column",
        type=str,
        default="net_earnings"
    )
    parser.add_argument(
        "--reward_scale",
        type=float,
        default=3000.0
    )
    parser.add_argument(
        "--train_fraction",
        type=float,
        default=0.2
    )
    parser.add_argument(
        "--hidden_units",
        type=int,
        default=32
    )
    parser.add_argument(
        "--alpha",
        type=float,
        default=1.0
    )
    parser.add_argument(
        "--ridge_alpha",
        type=float,
        default=10.0
    )
    parser.add_argument(
        "--max_iter",
        type=int,
        default=5000
    )
    parser.add_argument(
        "--validation_fraction",
        type=float,
        default=0.25
    )
    parser.add_argument(
        "--model_family",
        type=str,
        default="auto"
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=1
    )
    parser.add_argument(
        "--task_output_path",
        type=str,
        default=""
    )
    parser.add_argument(
        "--summary_output_path",
        type=str,
        default=""
    )
    parser.add_argument(
        "--model_output_path",
        type=str,
        default=""
    )
    parser.add_argument(
        "--metadata_output_path",
        type=str,
        default=""
    )
    args = parser.parse_args()

    script_dir = os.path.dirname(os.path.abspath(__file__))
    results_path = resolve_path(
        path=args.results_path,
        base_dir=script_dir
    )
    template_path = resolve_path(
        path=args.template_path,
        base_dir=script_dir
    )

    safe_source = slugify(args.source)
    default_base = os.path.join(
        script_dir,
        "..",
        "..",
        "optimal-nudging",
        "model",
        "results"
    )
    task_output_path = resolve_output_path(
        path=args.task_output_path,
        default_path=os.path.join(
            default_base,
            "optimal_nudging_ai_matched_data",
            "%s__mlp.csv" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    summary_output_path = resolve_output_path(
        path=args.summary_output_path,
        default_path=os.path.join(
            default_base,
            "optimal_nudging_ai_matched_summary",
            "%s__mlp.csv" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    model_output_path = resolve_output_path(
        path=args.model_output_path,
        default_path=os.path.join(
            default_base,
            "agent_nudgers",
            "%s__mlp.pkl" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    metadata_output_path = resolve_output_path(
        path=args.metadata_output_path,
        default_path=os.path.join(
            default_base,
            "agent_nudgers",
            "%s__mlp.json" % (safe_source)
        ),
        base_dir=os.getcwd()
    )

    template = pd.read_csv(template_path)
    template = template[~template["is_practice"]].copy()
    template_info = build_template_info(template=template)

    results = pd.read_csv(results_path)
    resolved_source = resolve_source(
        source=args.source,
        available_sources=sorted(results["source"].dropna().unique().tolist())
    )
    results = results[
        (results["nudge"] == "optimal")
        & (~results["is_practice"])
        & (results["source"] == resolved_source)
        & (results["nudge_type"].isin(args.train_nudge_type))
    ].copy()
    assert results.shape[0] > 0, "No optimal training rows found for source %s" % (resolved_source)
    results = attach_problem_ids(
        df=results,
        template_lookup=template_info["lookup"]
    )

    problem_ids = sorted(results["problem_id"].unique().tolist())
    assert len(problem_ids) >= 2, "Need at least 2 problem_ids, got %s for source %s" % (
        len(problem_ids),
        resolved_source
    )
    train_ids, test_ids = split_problem_ids(
        problem_ids=problem_ids,
        train_fraction=args.train_fraction,
        seed=args.seed
    )
    fit_ids, val_ids = split_problem_ids(
        problem_ids=train_ids,
        train_fraction=1.0 - args.validation_fraction,
        seed=args.seed + 1
    )
    assert len(fit_ids) > 0, "Fit split is empty for source %s" % (resolved_source)
    assert len(val_ids) > 0, "Validation split is empty for source %s" % (resolved_source)
    fit_df = results[results["problem_id"].isin(fit_ids)].copy()
    val_df = results[results["problem_id"].isin(val_ids)].copy()
    train_df = results[results["problem_id"].isin(train_ids)].copy()
    test_df = results[results["problem_id"].isin(test_ids)].copy()
    assert fit_df.shape[0] > 0, "Fit split is empty for source %s" % (resolved_source)
    assert val_df.shape[0] > 0, "Validation split is empty for source %s" % (resolved_source)
    assert train_df.shape[0] > 0, "Training split is empty for source %s" % (resolved_source)
    assert test_df.shape[0] > 0, "Test split is empty for source %s" % (resolved_source)

    x_fit, y_fit, fit_added, fit_candidates = build_learning_arrays(
        df=fit_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_val, y_val, val_added, val_candidates = build_learning_arrays(
        df=val_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_train, y_train, train_added, train_candidates = build_learning_arrays(
        df=train_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_test, y_test, test_added, test_candidates = build_learning_arrays(
        df=test_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )

    candidate_specs = build_candidate_specs(
        model_family=args.model_family,
        hidden_units=args.hidden_units,
        alpha=args.alpha,
        ridge_alpha=args.ridge_alpha,
        max_iter=args.max_iter,
        seed=args.seed
    )
    selection_rows = []
    best_name = ""
    best_metrics = None
    best_score = None
    for name, spec in candidate_specs.items():
        candidate_model = fit_model(
            spec=spec,
            x=x_fit,
            y=y_fit
        )
        val_pred = candidate_model.predict(x_val)
        metrics = evaluate_predictions(
            truth=y_val,
            pred=val_pred,
            added_cells=val_added,
            candidate_cells=val_candidates
        )
        selection_rows.append(
            {
                "model_name": name,
                **metrics
            }
        )
        score = -metrics["observed_set_reward_mae"]
        if best_score is None or score > best_score:
            best_score = score
            best_name = name
            best_metrics = metrics

    best_spec = candidate_specs[best_name]
    model = fit_model(
        spec=best_spec,
        x=x_train,
        y=y_train
    )

    train_pred = model.predict(x_train)
    test_pred = model.predict(x_test)
    metadata = build_metadata(
        source=resolved_source,
        reward_column=args.reward_column,
        reward_scale=args.reward_scale,
        fit_ids=fit_ids,
        val_ids=val_ids,
        train_ids=train_ids,
        test_ids=test_ids,
        x_train=x_train,
        x_test=x_test,
        y_train=y_train,
        y_test=y_test,
        train_pred=train_pred,
        test_pred=test_pred,
        train_added=train_added,
        train_candidates=train_candidates,
        test_added=test_added,
        test_candidates=test_candidates,
        selected_model=best_name,
        validation_metrics=best_metrics,
        candidate_validation_metrics=selection_rows
    )

    heldout_rows = template[
        (template["nudge_type"] == args.scaffold_nudge_type)
        & (template["problem_id"].isin(test_ids))
    ].copy()
    assert heldout_rows.shape[0] > 0, "No held-out scaffold rows matched test problem_ids"
    heldout_task, heldout_summary = generate_heldout_tasks(
        heldout_rows=heldout_rows,
        model=model,
        baseline_by_problem=template_info["baseline_by_problem"],
        output_nudge_type=args.output_nudge_type
    )

    for path in [
        task_output_path,
        summary_output_path,
        model_output_path,
        metadata_output_path
    ]:
        os.makedirs(os.path.dirname(path), exist_ok=True)

    heldout_task.to_csv(task_output_path, index=False)
    heldout_summary.to_csv(summary_output_path, index=False)
    with open(model_output_path, "wb") as outfile:
        pickle.dump(model, outfile)
    with open(metadata_output_path, "w") as outfile:
        json.dump(metadata, outfile, indent=2)

    print("Wrote held-out task CSV to %s" % (task_output_path))
    print("Wrote held-out summary CSV to %s" % (summary_output_path))
    print("Wrote nudger model to %s" % (model_output_path))
    print("Wrote metadata to %s" % (metadata_output_path))
    print("Selected model=%s Validation reward MAE=%.2f top3_overlap=%.3f" % (
        metadata["selected_model"],
        metadata["validation_metrics"]["observed_set_reward_mae"],
        metadata["validation_metrics"]["top3_overlap"]
    ))
    print(
        "Train rows=%s Test rows=%s Train problem_ids=%s Test problem_ids=%s" % (
            x_train.shape[0],
            x_test.shape[0],
            len(train_ids),
            len(test_ids)
        )
    )
    print(
        "Train reward mean=%.2f points MAE=%.2f R2=%.4f top3_overlap=%.3f" % (
            metadata["train_metrics"]["observed_set_reward_mean"],
            metadata["train_metrics"]["observed_set_reward_mae"],
            metadata["train_metrics"]["observed_set_reward_r2"],
            metadata["train_metrics"]["top3_overlap"]
        )
    )
    print(
        "Test reward mean=%.2f points MAE=%.2f R2=%.4f top3_overlap=%.3f" % (
            metadata["test_metrics"]["observed_set_reward_mean"],
            metadata["test_metrics"]["observed_set_reward_mae"],
            metadata["test_metrics"]["observed_set_reward_r2"],
            metadata["test_metrics"]["top3_overlap"]
        )
    )


# Data loading
def resolve_path(
    path: str,
    base_dir: str
) -> str:
    if os.path.isabs(path):
        return path
    return os.path.abspath(os.path.join(base_dir, path))


def resolve_output_path(
    path: str,
    default_path: str,
    base_dir: str
) -> str:
    if path == "":
        return os.path.abspath(default_path)
    return resolve_path(
        path=path,
        base_dir=base_dir
    )


def slugify(value: str) -> str:
    return value.replace("/", "__")


def resolve_source(
    source: str,
    available_sources: list[str]
) -> str:
    if source in available_sources:
        return source
    alt_source = source.replace("__", "/")
    if alt_source in available_sources:
        return alt_source
    raise AssertionError(
        "Unknown source %s. Available sources: %s" % (
            source,
            ", ".join(available_sources)
        )
    )


def parse_array(value: str) -> list:
    return ast.literal_eval(value)


def canonical_value(value: str) -> str:
    parsed = parse_array(value)
    return json.dumps(parsed, separators=(",", ":"))


def row_key(row: pd.Series) -> tuple[str, str, str, str]:
    return (
        canonical_value(row["payoff_matrix"]),
        canonical_value(row["cost_matrix"]),
        canonical_value(row["weights"]),
        str(row["nudge_type"])
    )


def attach_problem_ids(
    df: pd.DataFrame,
    template_lookup: dict[tuple[str, str, str, str], int]
) -> pd.DataFrame:
    if df["problem_id"].notna().all():
        df["problem_id"] = df["problem_id"].astype(int)
        return df

    attached = []
    for _, row in df.iterrows():
        key = row_key(row)
        assert key in template_lookup, "Could not recover problem_id for row"
        attached.append(template_lookup[key])
    df = df.copy()
    df["problem_id"] = attached
    return df


def build_template_info(
    template: pd.DataFrame
) -> dict[str, dict]:
    dedup = template[
        ["payoff_matrix", "cost_matrix", "weights", "nudge_type", "problem_id"]
    ].drop_duplicates()
    lookup = {}
    for _, row in dedup.iterrows():
        key = row_key(row)
        if key in lookup:
            assert lookup[key] == int(row["problem_id"])
        lookup[key] = int(row["problem_id"])

    baseline_by_problem = {}
    for problem_id, group in template.groupby("problem_id"):
        baseline_by_problem[int(problem_id)] = infer_baseline(group=group)

    return {
        "lookup": lookup,
        "baseline_by_problem": baseline_by_problem
    }


def infer_baseline(
    group: pd.DataFrame
) -> dict[str, np.ndarray]:
    zero_sets = []
    base_cost = None
    payoffs = None
    for cost_string in group["cost_matrix"].drop_duplicates():
        costs = np.array(parse_array(cost_string), dtype=int)
        zero_sets.append(set(np.flatnonzero(costs == 0).tolist()))
        if base_cost is None:
            base_cost = int(costs.max())
    payoffs = np.array(parse_array(group["payoff_matrix"].iloc[0]), dtype=float)
    common = sorted(list(set.intersection(*zero_sets)))
    assert len(common) >= 3
    baseline_flat = common[:3]
    baseline_mask = np.zeros(payoffs.size, dtype=int)
    baseline_mask[baseline_flat] = 1
    baseline_costs = np.full(payoffs.size, base_cost, dtype=int)
    baseline_costs[baseline_flat] = 0
    return {
        "payoffs": payoffs,
        "baseline_mask": baseline_mask,
        "baseline_costs": baseline_costs.reshape(payoffs.shape),
        "base_cost": base_cost
    }


# Learning arrays
def split_problem_ids(
    problem_ids: list[int],
    train_fraction: float,
    seed: int
) -> tuple[list[int], list[int]]:
    assert 0 < train_fraction < 1
    rng = random.Random(seed)
    shuffled = list(problem_ids)
    rng.shuffle(shuffled)
    n_train = max(1, int(round(train_fraction * len(shuffled))))
    n_train = min(n_train, len(shuffled) - 1)
    train_ids = sorted(shuffled[:n_train])
    test_ids = sorted(shuffled[n_train:])
    return train_ids, test_ids


def build_candidate_specs(
    model_family: str,
    hidden_units: int,
    alpha: float,
    ridge_alpha: float,
    max_iter: int,
    seed: int
) -> dict[str, dict]:
    allowed = ["auto", "ridge", "mlp"]
    assert model_family in allowed, "Unknown model_family %s" % (model_family)
    specs = {}
    if model_family in ["auto", "ridge"]:
        specs["ridge"] = {
            "name": "ridge",
            "estimator": Pipeline(
                [
                    ("scaler", StandardScaler()),
                    ("ridge", Ridge(alpha=ridge_alpha))
                ]
            )
        }
    if model_family in ["auto", "mlp"]:
        specs["mlp"] = {
            "name": "mlp",
            "estimator": Pipeline(
                [
                    ("scaler", StandardScaler()),
                    (
                        "mlp",
                        MLPRegressor(
                            hidden_layer_sizes=(hidden_units,),
                            solver="lbfgs",
                            alpha=alpha,
                            random_state=seed,
                            max_iter=max_iter
                        )
                    )
                ]
            )
        }
    return specs


def fit_model(
    spec: dict,
    x: np.ndarray,
    y: np.ndarray
) -> TransformedTargetRegressor:
    model = TransformedTargetRegressor(
        regressor=spec["estimator"],
        transformer=StandardScaler()
    )
    model.fit(x, y)
    return model


def build_cell_features(
    payoffs: np.ndarray,
    weights: np.ndarray,
    baseline_mask: np.ndarray,
    cell_index: int
) -> np.ndarray:
    n_features, n_options = payoffs.shape
    row = cell_index // n_options
    col = cell_index % n_options
    baseline_grid = baseline_mask.reshape(payoffs.shape)
    weighted_totals = weights @ payoffs
    row_means = payoffs.mean(axis=1)
    col_means = payoffs.mean(axis=0)
    row_visible = baseline_grid.sum(axis=1).astype(float)
    col_visible = baseline_grid.sum(axis=0).astype(float)
    best_total = float(np.max(weighted_totals))
    other_totals = np.delete(weighted_totals, col)
    best_other_total = float(np.max(other_totals))
    row_one_hot = np.zeros(n_features, dtype=float)
    col_one_hot = np.zeros(n_options, dtype=float)
    row_one_hot[row] = 1.0
    col_one_hot[col] = 1.0

    cell_count = float(payoffs[row, col])
    cell_weight = float(weights[row])
    cell_points = cell_count * cell_weight
    cell_extremity = abs(cell_count - 5.0)
    col_total = float(weighted_totals[col])
    col_gap_from_best = col_total - best_other_total
    is_best_col = float(col_total == best_total)

    return np.concatenate(
        [
            row_one_hot,
            col_one_hot,
            weights.astype(float),
            row_means.astype(float),
            col_means.astype(float),
            weighted_totals.astype(float),
            row_visible,
            col_visible,
            np.array(
                [
                    cell_count,
                    cell_weight,
                    cell_points,
                    cell_extremity,
                    float(row_means[row]),
                    float(col_means[col]),
                    col_total,
                    col_gap_from_best,
                    is_best_col
                ],
                dtype=float
            )
        ]
    )


def build_learning_arrays(
    df: pd.DataFrame,
    baseline_by_problem: dict[int, dict[str, np.ndarray]],
    reward_column: str,
    reward_scale: float
) -> tuple[np.ndarray, np.ndarray, list[list[int]], list[list[int]]]:
    x_rows = []
    y_rows = []
    added_cells = []
    candidate_cells = []

    for _, row in df.iterrows():
        problem_id = int(row["problem_id"])
        baseline = baseline_by_problem[problem_id]
        payoffs = np.array(parse_array(row["payoff_matrix"]), dtype=float)
        weights = np.array(parse_array(row["weights"]), dtype=float)
        costs = np.array(parse_array(row["cost_matrix"]), dtype=int)
        observed_mask = (costs.flatten() == 0).astype(int)
        added = sorted(
            np.flatnonzero((observed_mask == 1) & (baseline["baseline_mask"] == 0)).tolist()
        )
        assert len(added) == 3
        reward = float(row[reward_column]) * reward_scale
        available = [
            cell_index
            for cell_index in range(payoffs.size)
            if baseline["baseline_mask"][cell_index] == 0
        ]
        assert len(available) == payoffs.size - int(np.sum(baseline["baseline_mask"]))
        row_targets = []
        for cell_index in available:
            x_rows.append(
                build_cell_features(
                    payoffs=payoffs,
                    weights=weights,
                    baseline_mask=baseline["baseline_mask"],
                    cell_index=cell_index
                )
            )
            target = reward / len(added) if cell_index in added else 0.0
            y_rows.append(target)
            row_targets.append(target)
        added_cells.append(added)
        candidate_cells.append(available)

    assert len(x_rows) > 0, "No usable learning rows after preprocessing"
    return (
        np.vstack(x_rows),
        np.array(y_rows, dtype=float),
        added_cells,
        candidate_cells
    )


def build_metadata(
    source: str,
    reward_column: str,
    reward_scale: float,
    fit_ids: list[int],
    val_ids: list[int],
    train_ids: list[int],
    test_ids: list[int],
    x_train: np.ndarray,
    x_test: np.ndarray,
    y_train: np.ndarray,
    y_test: np.ndarray,
    train_pred: np.ndarray,
    test_pred: np.ndarray,
    train_added: list[list[int]],
    train_candidates: list[list[int]],
    test_added: list[list[int]],
    test_candidates: list[list[int]],
    selected_model: str,
    validation_metrics: dict,
    candidate_validation_metrics: list[dict]
) -> dict:
    return {
        "source": source,
        "reward_column": reward_column,
        "reward_scale": reward_scale,
        "n_fit_problem_ids": len(fit_ids),
        "n_validation_problem_ids": len(val_ids),
        "n_train_problem_ids": len(train_ids),
        "n_test_problem_ids": len(test_ids),
        "n_train_rows": int(x_train.shape[0]),
        "n_test_rows": int(x_test.shape[0]),
        "fit_problem_ids": fit_ids,
        "validation_problem_ids": val_ids,
        "train_problem_ids": train_ids,
        "test_problem_ids": test_ids,
        "selected_model": selected_model,
        "validation_metrics": validation_metrics,
        "candidate_validation_metrics": candidate_validation_metrics,
        "train_metrics": evaluate_predictions(
            truth=y_train,
            pred=train_pred,
            added_cells=train_added,
            candidate_cells=train_candidates
        ),
        "test_metrics": evaluate_predictions(
            truth=y_test,
            pred=test_pred,
            added_cells=test_added,
            candidate_cells=test_candidates
        )
    }


def evaluate_predictions(
    truth: np.ndarray,
    pred: np.ndarray,
    added_cells: list[list[int]],
    candidate_cells: list[list[int]]
) -> dict[str, float]:
    truth_reward = []
    pred_reward = []
    overlaps = []
    offset = 0
    for added, available in zip(added_cells, candidate_cells):
        next_offset = offset + len(available)
        truth_slice = truth[offset:next_offset]
        pred_slice = pred[offset:next_offset]
        available_positions = {cell: idx for idx, cell in enumerate(available)}
        added_positions = [available_positions[cell] for cell in added]
        truth_reward.append(float(np.sum(truth_slice[added_positions])))
        pred_reward.append(float(np.sum(pred_slice[added_positions])))
        top3_positions = np.argsort(-pred_slice)[:3].tolist()
        predicted_cells = [available[idx] for idx in top3_positions]
        overlaps.append(len(set(predicted_cells) & set(added)) / 3.0)
        offset = next_offset

    truth_reward = np.array(truth_reward, dtype=float)
    pred_reward = np.array(pred_reward, dtype=float)
    overlap = np.mean(
        overlaps
    )
    return {
        "cell_target_mae": float(mean_absolute_error(truth, pred)),
        "observed_set_reward_mean": float(np.mean(truth_reward)),
        "observed_set_reward_mae": float(mean_absolute_error(truth_reward, pred_reward)),
        "observed_set_reward_r2": float(r2_score(truth_reward, pred_reward)),
        "top3_overlap": float(overlap)
    }


# Held-out task generation
def generate_heldout_tasks(
    heldout_rows: pd.DataFrame,
    model: TransformedTargetRegressor,
    baseline_by_problem: dict[int, dict[str, np.ndarray]],
    output_nudge_type: str
) -> tuple[pd.DataFrame, pd.DataFrame]:
    output_rows = heldout_rows.copy()
    summary_rows = []
    new_cost_matrices = []

    for _, row in output_rows.iterrows():
        problem_id = int(row["problem_id"])
        baseline = baseline_by_problem[problem_id]
        payoffs = np.array(parse_array(row["payoff_matrix"]), dtype=float)
        weights = np.array(parse_array(row["weights"]), dtype=float)
        greedy_costs = np.array(parse_array(row["cost_matrix"]), dtype=int)
        greedy_mask = (greedy_costs.flatten() == 0).astype(int)
        greedy_added = sorted(
            np.flatnonzero((greedy_mask == 1) & (baseline["baseline_mask"] == 0)).tolist()
        )
        available = [
            cell_index
            for cell_index in range(payoffs.size)
            if baseline["baseline_mask"][cell_index] == 0
        ]
        features = np.vstack(
            [
                build_cell_features(
                    payoffs=payoffs,
                    weights=weights,
                    baseline_mask=baseline["baseline_mask"],
                    cell_index=cell_index
                )
                for cell_index in available
            ]
        )
        scores = model.predict(features)
        top3_positions = np.argsort(-scores)[:3].tolist()
        selected = [available[idx] for idx in top3_positions]

        new_costs = baseline["baseline_costs"].copy().flatten()
        new_costs[selected] = 0
        new_costs = new_costs.reshape(payoffs.shape)
        new_cost_matrices.append(json.dumps(new_costs.astype(int).tolist(), separators=(",", ":")))

        summary_rows.append(
            {
                "problem_id": problem_id,
                "participant_id": int(row["participant_id"]),
                "trial_num": int(row["trial_num"]),
                "trial_index": int(row["trial_index"]),
                "selected_cells": json.dumps(selected),
                "greedy_selected_cells": json.dumps(greedy_added),
                "overlap_with_greedy": int(len(set(selected) & set(greedy_added))),
                "predicted_reward": float(np.sum(scores[top3_positions]))
            }
        )

    output_rows["cost_matrix"] = new_cost_matrices
    output_rows["nudge_type"] = output_nudge_type
    output_rows["problem_id"] = output_rows["problem_id"].astype(str)
    blank_outcomes(df=output_rows)
    output_rows = output_rows.sort_values(["participant_id", "trial_num", "trial_index"]).reset_index(drop=True)

    summary = pd.DataFrame(summary_rows).sort_values(
        ["participant_id", "trial_num", "trial_index"]
    ).reset_index(drop=True)
    summary["problem_id"] = summary["problem_id"].astype(str)
    return output_rows, summary


def blank_outcomes(
    df: pd.DataFrame
) -> None:
    if "click_values" in df.columns:
        df["click_values"] = "[]"
    for column in [
        "selected_option",
        "gross_earnings",
        "net_earnings",
        "click_cost",
        "num_values_revealed",
        "reaction_time",
        "points_click_cost",
        "points_action_utility",
        "points_metalevel_reward",
        "time_elapsed"
    ]:
        if column in df.columns:
            df[column] = np.nan
    if "uncovered_values" in df.columns:
        df["uncovered_values"] = "[]"


if __name__ == "__main__":
    main()
