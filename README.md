# AI Agents Are Sensitive to Nudges

> [!NOTE]
> Code for the paper **[AI Agents Are Sensitive to Nudges](https://arxiv.org/abs/2505.11584)**. An initial version was submitted to the **NeurIPS Behavioral Machine Learning 2024**, and was accepted as a spotlight paper **[Superficial Alignment, Subtle Divergence, and Nudge Sensitivity in LLM Decision-Making](https://openreview.net/forum?id=chbdsWgjS6)**.

Large language models (LLMs) are increasingly deployed as autonomous agents that make choices and use tools on behalf of users. Yet, we have limited evidence about how their decisions are shaped by their environment. We adapt a human decision-making task to test leading LLMs under four forms of \textit{choice architecture}: defaults, suggestions, information highlighting, and ``optimal'' nudges derived from a resource-rational model of human choice. We treat human behavior as a baseline for predictable sensitivity to such interventions. Across models and prompting strategies, LLMs often depart substantially from this baseline. They sometimes pay excessive costs to acquire information, sometimes ignore available information, and, most crucially, are far more responsive to nudges than humans, such that weak cues that slightly shift human behavior have larger effects on model choices, toward both better and worse outcomes. Chain-of-thought prompting and in-context human data do not reliably stabilize behavior. Recent reasoning-optimized LLMs can, in some configurations, restore more human-level sensitivity to nudges, but do so inconsistently and at substantial computational cost. These results point to an important and largely neglected safety concern: LLM agents can be behaviorally brittle under subtle changes in choice architecture, even in the absence of adversarial settings.

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
@inproceedings{cherep2024superficial,
  title={Superficial Alignment, Subtle Divergence, and Nudge Sensitivity in LLM Decision-Making},
  author={Cherep, Manuel and Singh, Nikhil and Maes, Patricia},
  booktitle={NeurIPS 2024 Workshop on Behavioral Machine Learning}
}
```

MC received the support of a fellowship from “la Caixa” Foundation (ID 100010434). The fellowship code is LCF/BQ/EU23/12010079. We thank Keyon Vafa, Ryan Liu, Katie Collins, and Matt Groh for their supportive comments; and the community at the NeurIPS Behavioral ML workshop.
