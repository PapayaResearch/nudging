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

import pandas as pd
import logging
import os
import hydra
import litellm
from datetime import datetime
from tqdm.auto import tqdm
from omegaconf import OmegaConf
from hydra.utils import instantiate
from config import Config
from utils.misc import print_config

config_store = hydra.core.config_store.ConfigStore.instance()
config_store.store(name="base_config", node=Config)

@hydra.main(config_path="conf", config_name="config")
def main(cfg: Config) -> None:
    OmegaConf.resolve(cfg)
    cfg_yaml = OmegaConf.to_yaml(cfg)
    print_config(cfg)

    ##############################################
    # Check compatibility with LiteLLM
    ##############################################

    assert litellm.supports_function_calling(model=cfg.general.model) == True

    ##############################################
    # Create results and log directories
    ##############################################

    current_date = datetime.now().strftime("%a-%b-%d-%Y_%I-%M-%S%p")

    base_dir = os.path.join(
        cfg.nudge.name,
        cfg.provider.name,
        cfg.general.model
    )

    results_dir = os.path.join(cfg.logging.results_dir, base_dir, current_date)
    results_file = os.path.join(results_dir, current_date + ".csv")

    log_dir = os.path.join(cfg.logging.log_dir, base_dir)
    log_file = os.path.join(log_dir, current_date + ".log")

    os.makedirs(log_dir, exist_ok=True)
    os.makedirs(results_dir, exist_ok=True)

    with open(os.path.join(results_dir, "cfg.yaml"), "w") as outfile:
        outfile.write(cfg_yaml)
    with open(os.path.join(log_dir, "cfg.yaml"), "w") as outfile:
        outfile.write(cfg_yaml)

    logging.basicConfig(
        filename=log_file,
        level=logging.INFO,  # You can change this to DEBUG, ERROR, etc.
        format="%(asctime)s - %(levelname)s - %(message)s"
    )

    ##############################################
    # Set up provider
    ##############################################

    with open(cfg.provider.key) as infile:
        os.environ[cfg.provider.key_name] = infile.read().strip()

    ##############################################
    # Instantiate nudge
    ##############################################

    nudge = instantiate(cfg.nudge.nudge)

    ##############################################
    # Run simulation
    ##############################################

    participants = nudge.data.participant_id.unique()[cfg.general.offset:cfg.general.offset+cfg.general.participants]

    # Few-shot learning
    if cfg.general.fewshot > 0:
        df_fewshot = nudge.get_fewshot_data(
            participants,
            cfg.general.fewshot
        )

    for pid in tqdm(participants):
        logging.info("PARTICIPANT: {}".format(pid))

        messages = [
            {"role": "system", "content": nudge.initial_prompt},
        ]

        # Quiz
        messages = nudge.run_quiz(messages)

        # Few-shot examples
        if cfg.general.fewshot > 0:
            _, messages = nudge.run_trials(
                df_fewshot,
                messages,
                is_practice=False,
                fewshot_learning=True
            )

        # Chain-of-thought
        if cfg.general.cot:
            messages.append({"role": "user", "content": "Let's think step by step"})

        # Practice games
        messages.append({"role": "user", "content": nudge.practice_prompt})
        results, practice_messages = nudge.run_trials(
            nudge.get_practice_data(pid),
            messages,
            is_practice=True,
            fewshot_learning=False
        )
        if cfg.general.include_practice:
            # TODO: This is only including the last game of practice
            # so run_trials needs to (like fewshot collect all messages)
            messages = practice_messages

        if not os.path.exists(results_file):
            pd.DataFrame(results).to_csv(results_file, mode='w', header=True, index=False)
        else:
            pd.DataFrame(results).to_csv(results_file, mode='a', header=False, index=False)

        # Test games
        messages.append({"role": "user", "content": nudge.test_prompt})
        results, _ = nudge.run_trials(
            nudge.get_test_data(pid),
            messages,
            is_practice=False,
            fewshot_learning=False
        )
        pd.DataFrame(results).to_csv(results_file, mode='a', header=False, index=False)

if __name__ == "__main__":
    main()
