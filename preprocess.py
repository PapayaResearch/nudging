import os
import glob
import pandas


def main():
    files = set(glob.glob("data/*.csv"))
    files.remove("data/default_data.csv")

    # Exclude the "random" participants
    df = pandas.read_csv("data/default_data.csv")
    df = df.groupby("participant_id").filter(
        lambda x: (x[x["trial_nudge"] == "control"]["uncovered_values"] == "[]").mean() <= 0.5
    )

    df_llms = pandas.concat(
        [
            pandas.read_csv(
                f
            ).assign(
                source=os.path.splitext(os.path.basename(f))[0]
            ) for f in files
        ]
    )

    # Exclude "random" equivalents from LLM data
    df_llms = df_llms[df_llms["participant_id"].isin(df.participant_id.unique())]

    # Trim to only those participants whose games were LLM simulated
    df = df[df.participant_id.isin(df_llms.participant_id.unique())]
    df["source"] = "real"

    # Combine the two datasets
    df = pandas.concat([df, df_llms])

    df["n_prizes"] = df.payoff_matrix.map(lambda x: len(eval(x)))
    df["n_baskets"] = df.payoff_matrix.map(lambda x: len(eval(x)[0]))

    print(
        df.groupby(["source", "trial_nudge"])[["chose_nudge", "accepted_default"]].mean()
    )

    print(
        df.groupby(["source", "trial_nudge"])[["gross_earnings", "net_earnings"]].mean()
    )

    df.to_csv("data.csv", index=False)


if __name__ == "__main__":
    main()
