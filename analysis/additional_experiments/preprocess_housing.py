import os
import glob
import argparse
import yaml
import numpy
import pandas
from tqdm.auto import tqdm


NUDGES = [
    "default",
    "suggestion",
    "highlight"
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--modeldata_dir",
        type=str,
        default="data/results-housing"
    )
    parser.add_argument(
        "--output_dir",
        type=str,
        default="data"
    )
    parser.add_argument(
        "--output_prefix",
        type=str,
        default="data-housing"
    )
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    for nudge in tqdm(NUDGES, desc="Processing housing nudges"):
        files = glob.glob(
            os.path.join(
                args.modeldata_dir,
                nudge,
                "**",
                "*.csv"
            ),
            recursive=True
        )
        files = [file for file in files if not file.endswith(".DS_Store")]

        if len(files) == 0:
            continue

        cfgs = [
            load_cfg(file) for file in tqdm(
                files,
                desc="Loading config files",
                total=len(files),
                leave=False
            )
        ]

        dfs = []

        for file, cfg in tqdm(
            zip(files, cfgs),
            desc="Processing files",
            total=len(files),
            leave=False
        ):
            df = pandas.read_csv(file)
            run_id = os.path.relpath(os.path.dirname(file), args.modeldata_dir)

            df["participant_id_raw"] = df["participant_id"]
            df["participant_id"] = run_id
            df["run_id"] = run_id

            add_token_columns(df)

            df["nudge"] = cfg["nudge"]["name"]
            df["cot"] = cfg["general"]["cot"]
            df["source"] = cfg["general"]["model"]
            if "reasoning_effort" in cfg["general"]:
                df["source"] += "_%s" % cfg["general"]["reasoning_effort"]
            df["fs"] = cfg["general"]["fewshot"]

            dfs.append(df)

        if len(dfs) == 0:
            continue

        df = pandas.concat(dfs, ignore_index=True)

        add_common_columns(df)

        if nudge == "highlight":
            add_highlight_columns(df)

        if nudge == "suggestion":
            add_suggestion_columns(df)

        output_path = os.path.join(
            args.output_dir,
            "%s-%s.csv" % (args.output_prefix, nudge)
        )
        df.to_csv(output_path, index=False)


def load_cfg(file: str) -> dict:
    cfg_path = os.path.join(
        os.path.dirname(file),
        "cfg.yaml"
    )

    with open(cfg_path, "r") as infile:
        return yaml.safe_load(infile)


def add_token_columns(df: pandas.DataFrame) -> None:
    token_cols = [
        "completion_tokens",
        "prompt_tokens",
        "total_tokens",
        "reasoning_tokens"
    ]

    for col in token_cols:
        if col not in df.columns:
            df[col] = df.index.map(lambda x: [])
        else:
            df[col] = df[col].map(eval)

    df["sum_reasoning_tokens"] = df["reasoning_tokens"].map(sum)
    df["mean_reasoning_tokens"] = df["reasoning_tokens"].map(
        lambda x: numpy.mean(x) if len(x) > 0 else 0
    )


def add_common_columns(df: pandas.DataFrame) -> None:
    df["n_prizes"] = df["payoff_matrix"].map(lambda x: len(eval(x)))
    df["n_baskets"] = df["payoff_matrix"].map(lambda x: len(eval(x)[0]))

    df["idiosyncracy"] = df["weights"].map(
        eval
    ).map(
        numpy.array
    ).map(
        lambda x: numpy.abs(x - numpy.full(len(x), numpy.mean(x))).sum()
    )

    df["n_uncovered"] = df["uncovered_values"].map(eval).map(len)
    df["optimal_option"] = df.apply(optimal_option, axis=1)


def add_highlight_columns(df: pandas.DataFrame) -> None:
    df["nudge_index"] = df["nudge_index"].astype(int)

    def highlight_reveals(row: pandas.Series) -> tuple[int, bool]:
        uncovered_values = eval(row["uncovered_values"])
        n_cols = row["n_baskets"]
        uncovered_value_rows = [value // n_cols for value in uncovered_values]
        reveal_hits = [value for value in uncovered_value_rows if value == row["nudge_index"]]

        is_first_index_nudged = False
        if len(uncovered_value_rows) > 0:
            is_first_index_nudged = uncovered_value_rows[0] == row["nudge_index"]

        return len(reveal_hits), is_first_index_nudged

    df["highlight_reveals"], df["is_first_index_nudged"] = zip(
        *df.apply(highlight_reveals, axis=1)
    )


    df["highlight_value"] = df.apply(
        lambda row: eval(row["payoff_matrix"])[row["nudge_index"]][row["selected_option"]],
        axis=1
    )


def add_suggestion_columns(df: pandas.DataFrame) -> None:
    df["first_selected_option"] = df["first_selected_option"].fillna(-1).astype(int)
    df["selected_option"] = df["selected_option"].astype(int)

    df["value_first_option_selected"] = df.apply(
        lambda row: option_value(
            row=row,
            option="first_selected_option"
        ),
        axis=1
    )
    df["value_final_option_selected"] = df.apply(
        lambda row: option_value(
            row=row,
            option="selected_option"
        ),
        axis=1
    )


def option_value(row: pandas.Series, option: str) -> float:
    payoff_matrix = numpy.array(eval(row["payoff_matrix"]))
    weight_vector = numpy.array(eval(row["weights"]))
    return numpy.dot(payoff_matrix[:, row[option]], weight_vector)


def optimal_option(row: pandas.Series) -> int:
    payoff_matrix = numpy.array(eval(row["payoff_matrix"]))
    weight_vector = numpy.array(eval(row["weights"]))
    return numpy.argmax(payoff_matrix.T @ weight_vector)


if __name__ == "__main__":
    main()
