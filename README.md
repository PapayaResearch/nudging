# LLM Agents Are Hypersensitive to Nudges

> [!NOTE]
> An initial version was submitted to the **NeurIPS Behavioral Machine Learning 2024**, and was accepted as a spotlight paper **[Superficial Alignment, Subtle Divergence, and Nudge Sensitivity in LLM Decision-Making](https://openreview.net/forum?id=chbdsWgjS6)**.

LLMs are being set loose in complex, real-world environments involving sequential decision-making and tool use. Often, this involves making choices on behalf of human users. However, not much is known about the distribution of such choices, and how susceptible they are to different choice architectures. We perform a case study with a few such LLM models on a multi-attribute tabular decision-making problem, under canonical nudges such as the default option, suggestions, and information highlighting, as well as additional prompting strategies. We show that, despite superficial similarities to human choice distributions, such models differ in subtle but important ways. First, they show much higher susceptibility to the nudges. Second, they diverge in points earned, being affected by factors like the idiosyncrasy of available prizes. Third, they diverge in information acquisition strategies: e.g. incurring substantial cost to reveal too much information, or selecting without revealing any. Moreover, we show that simple prompt strategies like zero-shot chain of thought (CoT) can shift the choice distribution, and few-shot prompting with human data can induce greater alignment. Yet, none of these methods resolve the sensitivity of these models to nudges. Finally, we show how optimal nudges optimized with a human resource-rational model can similarly increase LLM performance for some models. All these findings suggest that behavioral tests are needed before deploying models as agents or assistants acting on behalf of users in complex environments.

> [!TIP]
> For more details about the original setup, we refer to [Optimal Nudging For Cognitively Bounded Agents](https://psycnet.apa.org/record/2024-21499-001).

## Installation

You can create the environment as follows

```bash
conda create -n nudging
conda activate nudging
pip install -r requirements.txt
```

## Providers

> [!IMPORTANT]
> We use [LiteLLM](https://www.litellm.ai/) to have access to 100+ LLMs in the OpenAI format. We suggest creating a `nudging/keys/` directory with an API key for each provider in an individual txt file (e.g. nudging/keys/oai.txt). You have access to this configuration in `nudging/conf/provider`.

## `nudging/`

Generating results is very simple

```bash
cd nudging
python main.py nudge=default general.model=gpt-4o-mini # nudge can be {default, suggestion, highlight, optimal}
```

## Configuration

> [!IMPORTANT]
> We use [Hydra](https://hydra.cc/) to configure everything. The configuration can be found in `nudging/conf/config.yaml`, with specific sub-configs in sub-directories of `nudging/conf/`.

The configs define all the parameters (e.g. nudge, provider, model, temperature). By default, these are the ones used for the paper. You can choose the `nudge` according to what's available in the configuration, a `provider` for different models (e.g openai, anthropic, google). This is also where you choose the `temperature`, the `fewshot` examples from human data, `cot` to activate chain-of-thought, `include_practice` to include these games in the context window, and the initial random `seed`.

## Analysis

The analysis R code is located in `analysis/`. It contains the results, and all scripts to run statistical tests and generate plots.

## Acknowledgements & Citing

Please cite this work as follows:
```bibtex
@article{cherep2025llm,
  title={LLM Agents Are Hypersensitive to Nudges},
  author={Cherep, Manuel and Maes, Pattie and Singh, Nikhil},
  journal={arXiv preprint arXiv:2505.11584},
  year={2025}
}
```

MC received the support of a fellowship from “la Caixa” Foundation (ID 100010434). The fellowship code is LCF/BQ/EU23/12010079. We thank Keyon Vafa, Ryan Liu, Katie Collins, and Matt Groh for their supportive comments; and the community at the NeurIPS Behavioral ML workshop.
