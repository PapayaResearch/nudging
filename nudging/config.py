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

from dataclasses import dataclass
from typing import Any

#######################
# Misc. Objects
#######################

@dataclass
class Nudge:
    # Nudge name
    name: str
    # Nudge object
    nudge: Any

@dataclass
class Provider:
    # Provider name
    name: str
    # Path to key txt file
    key: str
    # Key name for this provider
    key_name: str
    # Use integers or strings in tool calls
    supports_integers: bool

#######################
# General Settings
#######################

@dataclass
class General:
    # Model from those available in the provider
    model: str
    # Allows parallel tool calls
    parallel_tool_calls: bool
    # Force model to use one or more tools (none, auto, required)
    tool_choice: Any
    # Maximum number of tokens in the response from the API
    max_tokens: int
    # Number of participants from the original data
    participants: int
    # Offset the number of participants to be able to dynamically collect more
    offset: int
    # Temperature for the model (OpenAI's default is 1.0)
    temperature: float
    # Number of examples the model sees before playing, or null
    fewshot: Any
    # Include chain-of-Thought prompt with the experiments
    cot: bool
    # Include practice in the context window or not
    include_practice: bool
    # Random seed for reproducibility
    seed: int
    # Drop params specific params that the model doesn't support
    additional_drop_params: list
    # Delay before API calls to avoid rate limits
    delay: int
    # API wrapper
    api_call: Any

#######################
# Logging Settings
#######################

@dataclass
class Logging:
    # Directory for writing logs (for Tensorboard)
    log_dir: str
    # Directory for writing results (for audio and parameters)
    results_dir: str

######################
# The Config
######################

@dataclass
class Config:
    # Nudge settings
    nudge: Nudge
    # Provider settings
    provider: Provider
    # General settings
    general: General
    # Logging settings
    logging: Logging
