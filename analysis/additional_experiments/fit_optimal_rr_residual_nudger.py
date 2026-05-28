import os
import random
import argparse
import ast
import json
import pickle
import numpy as np
import pandas as pd
from sklearn.compose import TransformedTargetRegressor
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_absolute_error, r2_score
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
        default=["random", "extreme", "greedy"]
    )
    parser.add_argument(
        "--scaffold_nudge_type",
        type=str,
        default="greedy"
    )
    parser.add_argument(
        "--output_nudge_type",
        type=str,
        default="optimal_ai_resid"
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
        "--ridge_alpha",
        type=float,
        default=1.0
    )
    parser.add_argument(
        "--alpha",
        type=float,
        default=1.0
    )
    parser.add_argument(
        "--hidden_units",
        type=int,
        default=32
    )
    parser.add_argument(
        "--max_iter",
        type=int,
        default=5000
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
            "%s__resid.csv" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    summary_output_path = resolve_output_path(
        path=args.summary_output_path,
        default_path=os.path.join(
            default_base,
            "optimal_nudging_ai_matched_summary",
            "%s__resid.csv" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    model_output_path = resolve_output_path(
        path=args.model_output_path,
        default_path=os.path.join(
            default_base,
            "agent_nudgers",
            "%s__resid.pkl" % (safe_source)
        ),
        base_dir=os.getcwd()
    )
    metadata_output_path = resolve_output_path(
        path=args.metadata_output_path,
        default_path=os.path.join(
            default_base,
            "agent_nudgers",
            "%s__resid.json" % (safe_source)
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
    results = results[
        results["problem_id"].isin(template_info["greedy_by_problem"].keys())
    ].copy()
    assert results.shape[0] > 0, "No rows remain after filtering to RR-covered problem_ids"

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
    fit_df = results[results["problem_id"].isin(fit_ids)].copy()
    val_df = results[results["problem_id"].isin(val_ids)].copy()
    train_df = results[results["problem_id"].isin(train_ids)].copy()
    test_df = results[results["problem_id"].isin(test_ids)].copy()
    assert fit_df.shape[0] > 0, "Fit split is empty for source %s" % (resolved_source)
    assert val_df.shape[0] > 0, "Validation split is empty for source %s" % (resolved_source)
    assert train_df.shape[0] > 0, "Training split is empty for source %s" % (resolved_source)
    assert test_df.shape[0] > 0, "Test split is empty for source %s" % (resolved_source)

    x_fit, y_fit, fit_keys = build_learning_arrays(
        df=fit_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        greedy_by_problem=template_info["greedy_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_val, y_val, val_keys = build_learning_arrays(
        df=val_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        greedy_by_problem=template_info["greedy_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_train, y_train, train_keys = build_learning_arrays(
        df=train_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        greedy_by_problem=template_info["greedy_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )
    x_test, y_test, test_keys = build_learning_arrays(
        df=test_df,
        baseline_by_problem=template_info["baseline_by_problem"],
        greedy_by_problem=template_info["greedy_by_problem"],
        reward_column=args.reward_column,
        reward_scale=args.reward_scale
    )

    candidate_specs = build_candidate_specs(
        model_family=args.model_family,
        ridge_alpha=args.ridge_alpha,
        alpha=args.alpha,
        hidden_units=args.hidden_units,
        max_iter=args.max_iter,
        seed=args.seed
    )
    best_name = ""
    best_metrics = None
    best_score = None
    selection_rows = []
    for name, spec in candidate_specs.items():
        candidate_model = fit_model(
            spec=spec,
            x=x_fit,
            y=y_fit
        )
        val_pred = candidate_model.predict(x_val)
        metrics = evaluate_regression(
            truth=y_val,
            pred=val_pred
        )
        selection_rows.append(
            {
                "model_name": name,
                **metrics
            }
        )
        score = -metrics["reward_mae"]
        if best_score is None or score > best_score:
            best_score = score
            best_name = name
            best_metrics = metrics

    model = fit_model(
        spec=candidate_specs[best_name],
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
        y_train=y_train,
        y_test=y_test,
        train_pred=train_pred,
        test_pred=test_pred,
        train_keys=train_keys,
        test_keys=test_keys,
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
    print("Wrote residual nudger model to %s" % (model_output_path))
    print("Wrote metadata to %s" % (metadata_output_path))
    print("Selected model=%s Validation reward MAE=%.2f R2=%.4f" % (
        metadata["selected_model"],
        metadata["validation_metrics"]["reward_mae"],
        metadata["validation_metrics"]["reward_r2"]
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
        "Train reward mean=%.2f points MAE=%.2f R2=%.4f" % (
            metadata["train_metrics"]["reward_mean"],
            metadata["train_metrics"]["reward_mae"],
            metadata["train_metrics"]["reward_r2"]
        )
    )
    print(
        "Test reward mean=%.2f points MAE=%.2f R2=%.4f" % (
            metadata["test_metrics"]["reward_mean"],
            metadata["test_metrics"]["reward_mae"],
            metadata["test_metrics"]["reward_r2"]
        )
    )


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


def context_key(
    problem_id: int,
    payoff_matrix: str,
    weights: str
) -> tuple[int, str, str]:
    return (
        int(problem_id),
        canonical_value(payoff_matrix),
        canonical_value(weights)
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

    greedy_by_problem = {}
    greedy_rows = template[template["nudge_type"] == "greedy"].copy()
    for problem_id, group in greedy_rows.groupby("problem_id"):
        problem_id = int(problem_id)
        baseline = baseline_by_problem[problem_id]
        added_sets = []
        for _, row in group.iterrows():
            costs = np.array(parse_array(row["cost_matrix"]), dtype=int)
            added_sets.append(
                tuple(
                    infer_added_cells(
                        costs=costs,
                        baseline_mask=baseline["baseline_mask"]
                    )
                )
            )
        unique_added = sorted(list(set(added_sets)))
        assert len(unique_added) == 1, "Greedy anchor is not unique for problem_id %s" % (problem_id)
        greedy_by_problem[problem_id] = list(unique_added[0])

    return {
        "lookup": lookup,
        "baseline_by_problem": baseline_by_problem,
        "greedy_by_problem": greedy_by_problem
    }


def infer_baseline(
    group: pd.DataFrame
) -> dict[str, np.ndarray]:
    zero_sets = []
    base_cost = None
    payoffs = np.array(parse_array(group["payoff_matrix"].iloc[0]), dtype=float)
    for cost_string in group["cost_matrix"].drop_duplicates():
        costs = np.array(parse_array(cost_string), dtype=int)
        zero_sets.append(set(np.flatnonzero(costs == 0).tolist()))
        if base_cost is None:
            base_cost = int(costs.max())
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


def infer_added_cells(
    costs: np.ndarray,
    baseline_mask: np.ndarray
) -> list[int]:
    observed_mask = (costs.flatten() == 0).astype(int)
    added = sorted(
        np.flatnonzero((observed_mask == 1) & (baseline_mask == 0)).tolist()
    )
    assert len(added) == 3
    return added


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
    ridge_alpha: float,
    alpha: float,
    hidden_units: int,
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


def describe_cell_set(
    payoffs: np.ndarray,
    weights: np.ndarray,
    cell_indices: list[int]
) -> dict[str, np.ndarray]:
    n_features, n_options = payoffs.shape
    weighted_totals = weights @ payoffs
    flat_payoffs = payoffs.flatten()
    rows = np.array([cell_index // n_options for cell_index in cell_indices], dtype=int)
    cols = np.array([cell_index % n_options for cell_index in cell_indices], dtype=int)
    cell_counts = flat_payoffs[cell_indices].astype(float)
    cell_weights = weights[rows].astype(float)
    cell_points = cell_counts * cell_weights
    extremity = np.abs(cell_counts - 5.0)
    col_totals = weighted_totals[cols].astype(float)
    best_col = int(np.argmax(weighted_totals))
    second_best_col = int(np.argsort(-weighted_totals)[1])
    worst_col = int(np.argmin(weighted_totals))
    row_counts = np.bincount(rows, minlength=n_features).astype(float)
    col_counts = np.bincount(cols, minlength=n_options).astype(float)
    return {
        "row_counts": row_counts,
        "col_counts": col_counts,
        "scalars": np.array(
            [
                float(np.sum(cell_points)),
                float(np.mean(cell_points)),
                float(np.max(cell_points)),
                float(np.min(cell_points)),
                float(np.sum(cell_counts)),
                float(np.mean(cell_counts)),
                float(np.max(cell_counts)),
                float(np.min(cell_counts)),
                float(np.sum(extremity)),
                float(np.mean(extremity)),
                float(np.max(extremity)),
                float(np.sum(col_totals)),
                float(np.mean(col_totals)),
                float(np.max(col_totals)),
                float(len(np.unique(rows))),
                float(len(np.unique(cols))),
                float(np.sum(cols == best_col)),
                float(np.sum(cols == second_best_col)),
                float(np.sum(cols == worst_col))
            ],
            dtype=float
        )
    }


def build_set_features(
    payoffs: np.ndarray,
    weights: np.ndarray,
    baseline_mask: np.ndarray,
    selected: list[int],
    anchor: list[int]
) -> np.ndarray:
    baseline_grid = baseline_mask.reshape(payoffs.shape)
    row_means = payoffs.mean(axis=1).astype(float)
    col_means = payoffs.mean(axis=0).astype(float)
    weighted_totals = (weights @ payoffs).astype(float)
    baseline_row_visible = baseline_grid.sum(axis=1).astype(float)
    baseline_col_visible = baseline_grid.sum(axis=0).astype(float)
    selected_desc = describe_cell_set(
        payoffs=payoffs,
        weights=weights,
        cell_indices=selected
    )
    anchor_desc = describe_cell_set(
        payoffs=payoffs,
        weights=weights,
        cell_indices=anchor
    )
    overlap = float(len(set(selected) & set(anchor)))
    swap_distance = float(3 - overlap)
    return np.concatenate(
        [
            weights.astype(float),
            row_means,
            col_means,
            weighted_totals,
            baseline_row_visible,
            baseline_col_visible,
            selected_desc["row_counts"],
            selected_desc["col_counts"],
            anchor_desc["row_counts"],
            anchor_desc["col_counts"],
            selected_desc["row_counts"] - anchor_desc["row_counts"],
            selected_desc["col_counts"] - anchor_desc["col_counts"],
            selected_desc["scalars"],
            anchor_desc["scalars"],
            selected_desc["scalars"] - anchor_desc["scalars"],
            np.array([overlap, swap_distance], dtype=float)
        ]
    )


def build_learning_arrays(
    df: pd.DataFrame,
    baseline_by_problem: dict[int, dict[str, np.ndarray]],
    greedy_by_problem: dict[int, list[int]],
    reward_column: str,
    reward_scale: float
) -> tuple[np.ndarray, np.ndarray, list[tuple[int, str, str]]]:
    x_rows = []
    y_rows = []
    row_keys = []
    for _, row in df.iterrows():
        problem_id = int(row["problem_id"])
        payoffs = np.array(parse_array(row["payoff_matrix"]), dtype=float)
        weights = np.array(parse_array(row["weights"]), dtype=float)
        costs = np.array(parse_array(row["cost_matrix"]), dtype=int)
        baseline = baseline_by_problem[problem_id]
        key = context_key(
            problem_id=problem_id,
            payoff_matrix=row["payoff_matrix"],
            weights=row["weights"]
        )
        assert problem_id in greedy_by_problem, "Missing RR anchor for problem_id %s" % (problem_id)
        observed_added = infer_added_cells(
            costs=costs,
            baseline_mask=baseline["baseline_mask"]
        )
        anchor_added = greedy_by_problem[problem_id]
        reward = float(row[reward_column]) * reward_scale
        x_rows.append(
            build_set_features(
                payoffs=payoffs,
                weights=weights,
                baseline_mask=baseline["baseline_mask"],
                selected=observed_added,
                anchor=anchor_added
            )
        )
        y_rows.append(reward)
        row_keys.append(key)

    assert len(x_rows) > 0, "No usable learning rows after preprocessing"
    return (
        np.vstack(x_rows),
        np.array(y_rows, dtype=float),
        row_keys
    )


def evaluate_regression(
    truth: np.ndarray,
    pred: np.ndarray
) -> dict[str, float]:
    return {
        "reward_mean": float(np.mean(truth)),
        "reward_mae": float(mean_absolute_error(truth, pred)),
        "reward_r2": float(r2_score(truth, pred))
    }


def build_metadata(
    source: str,
    reward_column: str,
    reward_scale: float,
    fit_ids: list[int],
    val_ids: list[int],
    train_ids: list[int],
    test_ids: list[int],
    y_train: np.ndarray,
    y_test: np.ndarray,
    train_pred: np.ndarray,
    test_pred: np.ndarray,
    train_keys: list[tuple[int, str, str]],
    test_keys: list[tuple[int, str, str]],
    selected_model: str,
    validation_metrics: dict[str, float],
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
        "n_train_rows": len(train_keys),
        "n_test_rows": len(test_keys),
        "fit_problem_ids": fit_ids,
        "validation_problem_ids": val_ids,
        "train_problem_ids": train_ids,
        "test_problem_ids": test_ids,
        "selected_model": selected_model,
        "validation_metrics": validation_metrics,
        "candidate_validation_metrics": candidate_validation_metrics,
        "train_metrics": evaluate_regression(
            truth=y_train,
            pred=train_pred
        ),
        "test_metrics": evaluate_regression(
            truth=y_test,
            pred=test_pred
        )
    }


def enumerate_one_swap_sets(
    baseline_mask: np.ndarray,
    anchor: list[int]
) -> list[list[int]]:
    available = [
        cell_index
        for cell_index in range(len(baseline_mask))
        if baseline_mask[cell_index] == 0
    ]
    candidates = {tuple(sorted(anchor))}
    for drop_cell in anchor:
        for add_cell in available:
            if add_cell in anchor:
                continue
            new_set = sorted([cell for cell in anchor if cell != drop_cell] + [add_cell])
            candidates.add(tuple(new_set))
    return [list(candidate) for candidate in sorted(candidates)]


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
        payoffs = np.array(parse_array(row["payoff_matrix"]), dtype=float)
        weights = np.array(parse_array(row["weights"]), dtype=float)
        greedy_costs = np.array(parse_array(row["cost_matrix"]), dtype=int)
        baseline = baseline_by_problem[problem_id]
        anchor_added = infer_added_cells(
            costs=greedy_costs,
            baseline_mask=baseline["baseline_mask"]
        )
        candidate_sets = enumerate_one_swap_sets(
            baseline_mask=baseline["baseline_mask"],
            anchor=anchor_added
        )
        features = np.vstack(
            [
                build_set_features(
                    payoffs=payoffs,
                    weights=weights,
                    baseline_mask=baseline["baseline_mask"],
                    selected=candidate_set,
                    anchor=anchor_added
                )
                for candidate_set in candidate_sets
            ]
        )
        scores = model.predict(features)
        best_index = int(np.argmax(scores))
        selected = candidate_sets[best_index]
        selected_set = set(selected)
        anchor_set = set(anchor_added)
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
                "greedy_selected_cells": json.dumps(anchor_added),
                "overlap_with_greedy": int(len(selected_set & anchor_set)),
                "swap_distance_from_greedy": int(3 - len(selected_set & anchor_set)),
                "predicted_reward": float(scores[best_index]),
                "predicted_reward_greedy": float(scores[candidate_sets.index(anchor_added)]),
                "predicted_gain_vs_greedy": float(scores[best_index] - scores[candidate_sets.index(anchor_added)]),
                "candidate_count": len(candidate_sets)
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
