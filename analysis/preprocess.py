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
    "highlight",
    "optimal"
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data_dir",
        type=str,
        default="../nudging/data",
        help="Directory containing the (original participant) data"
    )
    parser.add_argument(
        "--modeldata_dir",
        type=str,
        default="./results",
        help="Directory containing the (agent generated) data"
    )
    args = parser.parse_args()

    os.makedirs("data", exist_ok=True)

    for nudge in tqdm(NUDGES, desc="Processing nudges"):
        files = glob.glob(
            os.path.join(
                args.modeldata_dir,
                nudge,
                "**",
                "*.csv"
            ),
            recursive=True
        )

        cfgs = [
            yaml.safe_load(
                open(
                    os.path.join(
                        os.path.dirname(f),
                        "cfg.yaml"
                    )
                )
            ) for f in tqdm(files, desc="Loading config files", total=len(files), leave=False)
        ]

        dfs = []

        for f, cfg in tqdm(zip(files, cfgs), desc="Processing files", total=len(files), leave=False):
            df = pandas.read_csv(f)

            token_cols = ["completion_tokens", "prompt_tokens", "total_tokens", "reasoning_tokens"]
            for col in token_cols:
                if col not in df.columns:
                    df[col] = df.index.map(lambda x: [])
                else:
                    df[col] = df[col].map(eval)
            
            df["sum_reasoning_tokens"] = df["reasoning_tokens"].map(sum)
            df["mean_reasoning_tokens"] = df["reasoning_tokens"].map(lambda x : numpy.mean(x) if len(x) > 0 else 0)

            df["nudge"] = cfg["nudge"]["name"]
            df["cot"] = cfg["general"]["cot"]
            df["source"] = cfg["general"]["model"]
            if "reasoning_effort" in cfg["general"]:
                df["source"] += "_%s" % cfg["general"]["reasoning_effort"]
            df["fs"] = cfg["general"]["fewshot"]

            dfs.append(df)

        df = pandas.concat(dfs, ignore_index=True)

        participant_ids = set(df["participant_id"].tolist())

        # Trim to only those participants whose games were LLM simulated

        df_participants = pandas.read_csv(
            os.path.join(
                args.data_dir,
                (nudge if nudge != "optimal" else "optimal_nudging_changing_belief_state_data") + ".csv"
            )
        )
        df_participants = df_participants[df_participants.participant_id.isin(participant_ids)]
        df_participants["nudge"] = nudge
        df_participants["cot"] = False
        df_participants["source"] = "real"
        df_participants["fs"] = 0

        if nudge == "highlight":
            df_participants["nudge_index"] = df_participants["highlight_index"]

        # Combine the two datasets
        df = pandas.concat([df, df_participants], axis=0, join="outer", ignore_index=True)

        df["n_prizes"] = df.payoff_matrix.map(lambda x: len(eval(x)))
        df["n_baskets"] = df.payoff_matrix.map(lambda x: len(eval(x)[0]))

        df["idiosyncracy"] = df.weights.map(
            eval
        ).map(
            numpy.array
        ).map(
            lambda x : numpy.abs(x - numpy.full(len(x), numpy.mean(x))).sum()
        )

        df["n_uncovered"] = df.uncovered_values.map(eval).map(len)

        if nudge == "highlight":
            df["nudge_index"] = df.nudge_index.astype(int)

            def highlight_reveals(row: pandas.Series) -> int:
                uncovered_values = eval(row.uncovered_values)
                uncovered_values = sorted(set(uncovered_values)) # Remove any duplicate reveals
                n_cols = row.n_baskets
                uncovered_valuerows = [v//n_cols for v in uncovered_values]
                highlight_reveals = [v for v in uncovered_valuerows if v == row.nudge_index]

                is_first_index_nudged = False
                if len(highlight_reveals) > 0:
                    is_first_index_nudged = highlight_reveals[0] == row.nudge_index

                assert len(highlight_reveals) <= n_cols, (highlight_reveals, n_cols, row.n_prizes)
                return len(highlight_reveals), is_first_index_nudged

            df["highlight_reveals"], df["is_first_index_nudged"] = zip(*df.apply(highlight_reveals, axis=1))
            df["highlight_value"] = df.apply(lambda row: eval(row.payoff_matrix)[row.nudge_index][row.selected_option], axis=1)


        if nudge == "suggestion":
            df["first_selected_option"] = df.first_selected_option.fillna(-1).astype(int)
            df["selected_option"] = df.selected_option.astype(int)

            def value_option(row: pandas.Series, option: str) -> float:
                payoff_matrix = numpy.array(eval(row.payoff_matrix))
                weight_vector = numpy.array(eval(row.weights))
                return numpy.dot(payoff_matrix[:, row[option]], weight_vector)

            df["value_first_option_selected"] = df.apply(lambda row: value_option(row, "first_selected_option"), axis=1)
            df["value_final_option_selected"] = df.apply(lambda row: value_option(row, "selected_option"), axis=1)

        def optimal_option(row: pandas.Series) -> int:
            payoff_matrix = numpy.array(eval(row.payoff_matrix))
            weight_vector = numpy.array(eval(row.weights))
            return numpy.argmax(payoff_matrix.T @ weight_vector)

        df["optimal_option"] = df.apply(optimal_option, axis=1)

        df.to_csv("data/data-%s.csv" % nudge, index=False)


if __name__ == "__main__":
    main()
