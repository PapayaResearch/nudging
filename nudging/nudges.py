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
import numpy as np
import ast
import random
import string
import json
import logging
from renderer import render, render_table, render_header, render_cost, render_result, render_tool_call
from litellm.types.utils import ChatCompletionMessageToolCall, Function

class MultiAttribute:
    def __init__(
            self,
            data: str,
            api_call: callable,
            quiz_choices: list,
            quiz_answers: list,
            initial_prompt: str,
            quiz_prompt: str,
            incorrect_quiz_prompt: str,
            practice_prompt: str,
            test_prompt: str,
            supports_integers: bool,
            seed: int
    ):
        # Load and shuffle data with reproducibility
        self.data = pd.read_csv(data).sample(frac=1, random_state=seed)
        self.api_call = api_call
        self.quiz_choices = quiz_choices
        self.quiz_answers = quiz_answers
        self.initial_prompt = initial_prompt
        self.quiz_prompt = quiz_prompt
        self.incorrect_quiz_prompt = incorrect_quiz_prompt
        self.practice_prompt = practice_prompt
        self.test_prompt = test_prompt
        self.supports_integers = supports_integers
        self.seed = seed

    def get_test_data(
            self,
            pid: int
    ):
        return self.data[(self.data.participant_id == pid)
                         & ~self.data.is_practice].sort_values(by="trial_num",
                                                               ascending=True)

    def get_practice_data(
            self,
            pid: int
    ):
        return self.data[(self.data.participant_id == pid)
                         & self.data.is_practice].sort_values(by="trial_num",
                                                              ascending=True)

    def get_control_tools(self, prizes, baskets):
        # Returns the tools for the control, the option to reveal or select
        prizes = [chr(65 + i) for i in prizes] # Letter instead of indices
        return [
            {
                "type": "function",
                "function": {
                    "name": "reveal",
                    "strict": True,
                    "description": "Call this whenever you choose to reveal the value of a box.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "prize": {
                                "type": "string",
                                "enum": prizes,
                                "description": "The prize's letter corresponding to the box.",
                            },
                            "basket": {
                                "type": "integer" if self.supports_integers else "string",
                                "enum": baskets if self.supports_integers else [str(v) for v in baskets],
                                "description": "The basket's number corresponding to the box.",
                            },
                        },
                        "required": ["prize", "basket"],
                        "additionalProperties": False,
                    },
                }
            },
            {
                "type": "function",
                "function": {
                    "name": "select",
                    "strict": True,
                    "description": "Call this whenever you choose to select a basket.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "basket": {
                                "type": "integer" if self.supports_integers else "string",
                                "enum": baskets if self.supports_integers else [str(v) for v in baskets],
                                "description": "The basket's number.",
                            },
                        },
                        "required": ["basket"],
                        "additionalProperties": False,
                    },
                }
            }
        ]

    def get_quiz_tools(self):
        properties  = {
            f"question_{i+1}": {
                "type": "integer" if self.supports_integers else "string",
                "enum": list(range(self.quiz_choices[i])) if self.supports_integers else [str(v) for v in range(self.quiz_choices[i])],
                "description": f"Answer for question {i+1} with the right choice's index.",
            }
            for i in range(len(self.quiz_choices))
        }
        return [
            {
                "type": "function",
                "function": {
                    "name": "quiz",
                    "strict": True,
                    "description": "Call this to answer the quiz.",
                    "parameters": {
                        "type": "object",
                        "properties": properties,
                        "required": [f"question_{i+1}" for i in range(len(self.quiz_choices))],
                        "additionalProperties": False,
                    },
                }
            }
        ]

    def run_quiz(self, messages):
        # Initialize state
        passed_quiz = False
        quiz_attempts = 0

        while not passed_quiz:
            # Ask questions
            messages.append({"role": "user", "content": self.quiz_prompt})
            response = self.api_call(messages, self.get_quiz_tools())

            # Parse response
            tool_call = response.choices[0].message.tool_calls[0]
            args = json.loads(tool_call.function.arguments)
            answers = [int(args.get(f"question_{i}")) for i in range (1, len(self.quiz_answers) + 1)]

            if answers == self.quiz_answers:
                logging.info("CORRECT QUIZ: {}".format(args))

                # Update state
                args["quiz"] = "You passed the quiz!"
                passed_quiz = True
            else:
                logging.info("INCORRECT QUIZ: {}".format(args))

                # Update state
                args["quiz"] = "You didn't pass the quiz."
                quiz_attempts += 1

            # Create a message containing the result of the function call
            tool_response, function_call_result_message = render_tool_call(tool_call, args)
            messages.append(tool_response)
            messages.append(function_call_result_message)

            if not passed_quiz:
                # Help message for the quiz
                messages.append({"role": "user", "content": self.incorrect_quiz_prompt})

            if quiz_attempts >= 3:
                # Some models might not be able to answer
                break

        return messages

class Default(MultiAttribute):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def get_fewshot_data(
            self,
            participants: list,
            n_examples: int
    ):
        # Remove participants already selected and practice trials
        df = self.data[~self.data.participant_id.isin(participants)
                       & ~self.data.is_practice]
        # Shuffle and drop duplicates
        df = df.sample(
            frac=1,
            random_state=self.seed
        ).drop_duplicates(['weights'])
        # Select default trials that were accepted and sample a subset
        df_default_true = df[(df.trial_nudge == "default")
                             & df.accepted_default]
        df_default_true = df_default_true.sample(
            n=n_examples // 4,
            random_state=self.seed
        )
        # Select default trials that were rejected, revealed at least one cell, and sample a subset
        df_default_false = df[(df.trial_nudge == "default")
                              & ~df.accepted_default
                              & (df.uncovered_values != "[]")]
        df_default_false = df_default_false.sample(
            n=n_examples // 4,
            random_state=self.seed
        )
        # Select control trials, revealed at least one cell, and sample a subset
        df_control = df[(df.trial_nudge == "control")
                        & (df.uncovered_values != "[]")]
        df_control = df_control.sample(
            n=n_examples // 2,
            random_state=self.seed
        )
        # Final dataframe of examples
        return pd.concat([df_default_true,
                          df_default_false,
                          df_control])

    def get_nudge_tools(self):
        return [
            {
                "type": "function",
                "function": {
                    "name": "default",
                    "strict": True,
                    "description": "Call this to accept or decline the default basket.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "decision": {
                                "type": "boolean",
                                "description": "Accept or decline the default basket.",
                            },
                        },
                        "required": ["decision"],
                        "additionalProperties": False,
                    },
                }
            }
        ]

    def render_nudge(self, idx):
        return f"Do you want to choose basket {idx+1}?\nIt's pays the most when the prizes are equally valuable."

    def run_trials(self, df, initial_messages, is_practice, fewshot_learning):
        # Global state
        total_earnings = 1.3 # dollars

        # Iterate through all (practice and test) games
        results = []
        for idx, row in enumerate(df.itertuples()):
            if (not fewshot_learning) or idx==0:
                # For regular games, messages only in the context
                # of the current game. For few-shot learning, all messages.
                messages = initial_messages.copy()

            # Game details
            payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
            revealed = np.full(payoff_matrix.shape, False)
            weights = np.array(ast.literal_eval(row.weights))
            nudge = row.trial_nudge == "default"
            nudge_index = int(row.nudge_index)
            reveal_cost = row.cost
            n_total_trials = df.shape[0]
            n_trial = (idx % n_total_trials) + 1

            # Game state
            selected_basket = None
            uncovered_values = []
            cost = 0
            net_earnings = 0
            gross_earnings = 0
            accepted_default = False
            chose_nudge = False

            # Format initial game
            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
            rendered_table = render_table(payoff_matrix, revealed, weights)
            rendered_cost = render_cost(cost)

            if nudge:
                game = render(
                    rendered_header,
                    self.render_nudge(nudge_index),
                    rendered_table,
                    rendered_cost
                )
                tools = self.get_nudge_tools()
            else:
                game = render(
                    rendered_header,
                    rendered_table,
                    rendered_cost
                )
                tools = self.get_control_tools(
                    list(range(len(weights))), # prize indices
                    list(range(1, payoff_matrix.shape[1]+1)) # basket indices
                )

            # Start interaction with initial game
            logging.info(game)
            messages.append({"role": "user", "content": game})

            if fewshot_learning:
                chose_default = False
                human_uncovered_values = ast.literal_eval(row.uncovered_values)

            # Interact until the end of game (i.e. basket is selected)
            while selected_basket is None:
                if not fewshot_learning:
                    response = self.api_call(messages=messages, tools=tools)

                    # Parse response
                    tool_call = response.choices[0].message.tool_calls[0]
                    action = tool_call.function.name
                    args = json.loads(tool_call.function.arguments)
                else:
                    # If few shot learning, ignore responses and simulate it
                    if row.trial_nudge == "default" and not chose_default:
                        chose_default = True
                        action = "default"
                        args = {"decision": row.accepted_default}
                    elif human_uncovered_values:
                        action = "reveal"
                        flat_index = human_uncovered_values.pop(0)
                        args = {"prize": chr((flat_index // payoff_matrix.shape[1]) + 65),
                                "basket": flat_index % payoff_matrix.shape[1] + 1}
                    else:
                        action = "select"
                        args = {"basket": row.selected_option+1}

                    tool_call = ChatCompletionMessageToolCall(
                        id="call_" + "".join([random.choice(string.ascii_letters + string.digits) for _ in range(24)]),
                        type="function",
                        function=Function(
                            name=action, arguments=json.dumps(args)
                        )
                    )

                if action == "reveal":
                    logging.info("REVEAL: {}".format(args))

                    # Update state
                    prize = ord(args.get("prize")) - 65 # Letter to index
                    basket_idx = int(args.get("basket")) - 1
                    uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                    revealed[prize, basket_idx] = True
                    cost += reveal_cost

                    # Prepare game details with the newly revealed box
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost)
                    )

                elif action == "select":
                    logging.info("SELECT: {}".format(args))

                    # Calculate game results
                    points = payoff_matrix[:, int(args.get("basket"))-1]
                    total_points = np.sum(points * weights)
                    gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                    net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01

                    # Update state
                    selected_basket = int(args.get("basket"))
                    if not is_practice:
                        total_earnings += net_earnings
                    chose_nudge = (selected_basket == (nudge_index + 1))
                    # Reveal the whole basket since it was selected
                    revealed[:, (int(args.get("basket"))-1)] = True

                    # Prepare game details with the earnings from the selected basket
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost),
                        render_result(weights, points, total_points, net_earnings)
                    )

                elif action == "default":
                    logging.info("DEFAULT: {}".format(args))

                    if args.get("decision"):
                        # Calculate game results
                        points = payoff_matrix[:, nudge_index]
                        total_points = np.sum(points * weights)
                        gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                        net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01

                        # Update state
                        selected_basket = nudge_index + 1
                        if not is_practice:
                            total_earnings += net_earnings
                        chose_nudge = True
                        accepted_default = True

                        # Prepare game details with the earnings from the selected basket
                        new_game = render(
                            render_header(
                                total_earnings,
                                is_practice,
                                n_trial,
                                n_total_trials
                            ),
                            render_table(payoff_matrix, revealed, weights),
                            render_cost(cost),
                            render_result(weights, points, total_points, net_earnings)
                        )
                    else:
                        # Update state
                        tools = self.get_control_tools(
                            list(range(len(weights))), # prize indices
                            list(range(1, payoff_matrix.shape[1]+1)) # basket indices
                        )

                        # Prepare game details with the newly revealed box
                        new_game = render(
                            render_header(
                                total_earnings,
                                is_practice,
                                n_trial,
                                n_total_trials
                            ),
                            render_table(payoff_matrix, revealed, weights),
                            render_cost(cost)
                        )

                args[action] = new_game
                logging.info(new_game)

                # Create a message containing the result of the function call
                tool_response, function_call_result_message = render_tool_call(tool_call, args)
                messages.append(tool_response)
                messages.append(function_call_result_message)

            # Save values
            if not fewshot_learning:
                results.append(
                    {
                        "is_practice": row.is_practice,
                        "cost": row.cost,
                        "payoff_matrix": row.payoff_matrix,
                        "weights": row.weights,
                        "trial_num": row.trial_num,
                        "trial_nudge": row.trial_nudge,
                        "nudge_index": row.nudge_index,
                        "nudge_type": row.nudge_type,
                        "participant_id": row.participant_id,
                        "selected_option": selected_basket-1,
                        "gross_earnings": gross_earnings,
                        "net_earnings": net_earnings,
                        "accepted_default": accepted_default,
                        "chose_nudge": chose_nudge,
                        "uncovered_values": uncovered_values
                     }
                )
        return results, messages

class Suggestion(MultiAttribute):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def get_fewshot_data(
            self,
            participants: list,
            n_examples: int
    ):
        # Remove participants already selected and practice trials
        df = self.data[~self.data.participant_id.isin(participants)
                       & ~self.data.is_practice]
        # Shuffle and drop duplicates
        df = df.sample(
            frac=1,
            random_state=self.seed
        ).drop_duplicates(['weights'])
        # Select pre-supersize trials, revealed at least one cell, and sample a subset
        df_pre = df[(df.trial_nudge == "pre-supersize")
                    & (df.uncovered_values != "[]")]
        df_pre = df_pre.sample(
            n=n_examples // 4,
            random_state=self.seed
        )
        # Select post-supersize trials, revealed at least one cell, and sample a subset
        df_post = df[(df.trial_nudge == "post-supersize")
                     & (df.uncovered_values != "[]")]
        df_post = df_post.sample(
            n=n_examples // 4,
            random_state=self.seed
        )
        # Select control trials, revealed at least one cell, and sample a subset
        df_control = df[(df.trial_nudge == "control")
                        & (df.uncovered_values != "[]")]
        df_control = df_control.sample(
            n=n_examples // 2,
            random_state=self.seed
        )
        # Final dataframe of examples
        return pd.concat([df_pre,
                          df_post,
                          df_control])

    def render_nudge(self, value, prize_idx, basket=None):
        prize = chr(prize_idx + 65)
        if basket is None:
            return f"We found another basket with {value} {prize} prizes!"
        else:
            return f"Consider basket {basket} - it has {value} {prize} prizes!"

    def get_best_basket(self, payoff_matrix, weights, human_uncovered_values):
        # Replicate payoff matrix masking with zeros the values that are not revealed
        M_flat = payoff_matrix.flatten()
        mask = np.zeros_like(M_flat)
        mask[human_uncovered_values] = 1
        result = (M_flat * mask).reshape(payoff_matrix.shape)
        basket_points_revealed = weights @ result[:, :-1] # revealed points per basket (ignoring last)
        # Selected basket is best according to information revealed or random by tiebreak
        selected_basket_idx = int(
            np.random.choice(
                np.flatnonzero(
                    basket_points_revealed == basket_points_revealed.max()
                )
            )
        )
        return selected_basket_idx+1

    def run_trials(self, df, initial_messages, is_practice, fewshot_learning):
        # Global state
        total_earnings = 1.3 # dollars

        # Iterate through all (practice and test) games
        results = []
        for idx, row in enumerate(df.itertuples()):
            if (not fewshot_learning) or idx==0:
                # For regular games, messages only in the context
                # of the current game. For few-shot learning, all messages.
                messages = initial_messages.copy()

            # Game details
            payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
            revealed = np.full(payoff_matrix.shape, False)
            weights = np.array(ast.literal_eval(row.weights))
            nudge = row.trial_nudge # control, post-supersize, or pre-supersize
            nudge_index = int(row.nudge_index)
            og_baskets = int(row.og_baskets)
            shown_baskets = int(row.shown_baskets)
            selected_option = int(row.selected_option)
            reveal_cost = row.cost
            n_total_trials = df.shape[0]
            n_trial = (idx % n_total_trials) + 1
            human_uncovered_values_original = ast.literal_eval(row.uncovered_values)
            human_uncovered_values = ast.literal_eval(row.uncovered_values)

            # Game state
            first_selected_basket = None
            selected_basket = None
            uncovered_values = []
            cost = 0
            net_earnings = 0
            gross_earnings = 0
            chose_nudge = False

            # Format initial game
            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
            rendered_cost = render_cost(cost)

            # Pre-render nudge because it stays there all the time
            if nudge in ["pre-supersize", "post-supersize"]:
                nudge_prize_idx = payoff_matrix[:, nudge_index].argmax() # idx of highest value in the basket
                nudge_value = payoff_matrix[nudge_prize_idx, nudge_index] # highest value in the basket
                nudge_basket = nudge_index + 1 if nudge == "pre-supersize" else None
                rendered_nudge = self.render_nudge(
                    nudge_value,
                    nudge_prize_idx,
                    nudge_basket
                )

            if nudge == "pre-supersize":
                revealed[nudge_prize_idx, nudge_basket-1] = True
                rendered_table = render_table(payoff_matrix, revealed, weights)
                game = render(
                    rendered_header,
                    rendered_nudge,
                    rendered_table,
                    rendered_cost
                )
            elif nudge == "post-supersize":
                revealed[nudge_prize_idx, -1] = True
                rendered_table = render_table(payoff_matrix[:, :og_baskets], revealed, weights)
                game = render(
                    rendered_header,
                    rendered_table,
                    rendered_cost
                )
            elif nudge == "control":
                rendered_table = render_table(payoff_matrix, revealed, weights)
                game = render(
                    rendered_header,
                    rendered_table,
                    rendered_cost
                )

            # Baskets should be limited to shown_baskets for control and pre-supersize;
            # and og_baskets or shown_baskets at different stages for post-supersize
            if nudge == "control" or nudge == "pre-supersize":
                tools = self.get_control_tools(
                    list(range(len(weights))), # prize indices
                    list(range(1, shown_baskets+1)) # basket indices
                )
            elif nudge == "post-supersize":
                tools = self.get_control_tools(
                    list(range(len(weights))), # prize indices
                    list(range(1, og_baskets+1)) # basket indices
                )

            # Start interaction with initial game
            logging.info(game)
            messages.append({"role": "user", "content": game})

            # Interact until the end of game (i.e. basket is selected)
            while selected_basket is None:
                if not fewshot_learning:
                    response = self.api_call(messages=messages, tools=tools)

                    # Parse response
                    tool_call = response.choices[0].message.tool_calls[0]
                    action = tool_call.function.name
                    args = json.loads(tool_call.function.arguments)
                else:
                    # If few shot learning, ignore responses and simulate it
                    if nudge == "post-supersize":
                        if human_uncovered_values:
                            flat_index = human_uncovered_values[0]
                            prize = chr((flat_index // payoff_matrix.shape[1]) + 65)
                            basket = flat_index % payoff_matrix.shape[1] + 1

                            if (basket != (nudge_index+1)) or first_selected_basket is not None:
                                # Reveal
                                human_uncovered_values.pop(0)
                                action = "reveal"
                                args = {"prize": prize,
                                        "basket": basket}
                            else:
                                # Basket selected after post suggestion happened, so
                                # a basket had to be selected before this reveal
                                action = "select"
                                if selected_option != nudge_index:
                                    # (a) If selected option is not the nudge, then assume that basket was selected both times
                                    args = {"basket": selected_option+1}
                                else:
                                    # (b) If selected option is nudge, then first basket is the best (known) one
                                    best_basket = self.get_best_basket(
                                        payoff_matrix,
                                        weights,
                                        human_uncovered_values_original
                                    )
                                    args = {"basket": best_basket}
                        else:
                            action = "select"
                            if (selected_option != nudge_index) or (first_selected_basket is not None):
                                # If selected option is not the nudge, then assume that basket was selected both times
                                # If this is the second time selecting, then selected option is the final choice
                                args = {"basket": selected_option+1}
                            else:
                                # Select the best basket according to the information
                                best_basket = self.get_best_basket(
                                    payoff_matrix,
                                    weights,
                                    human_uncovered_values_original
                                )
                                args = {"basket": best_basket}
                    else:
                        if human_uncovered_values:
                            action = "reveal"
                            flat_index = human_uncovered_values.pop(0)
                            args = {"prize": chr((flat_index // payoff_matrix.shape[1]) + 65),
                                    "basket": flat_index % payoff_matrix.shape[1] + 1}
                        else:
                            action = "select"
                            args = {"basket": selected_option+1}

                    tool_call = ChatCompletionMessageToolCall(
                        id="call_" + "".join([random.choice(string.ascii_letters + string.digits) for _ in range(24)]),
                        type="function",
                        function=Function(
                            name=action, arguments=json.dumps(args)
                        )
                    )

                if action == "reveal":
                    logging.info("REVEAL: {}".format(args))

                    # Update state
                    prize = ord(args.get("prize")) - 65 # Letter to index
                    basket_idx = int(args.get("basket")) - 1
                    uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                    revealed[prize, basket_idx] = True
                    cost += reveal_cost

                    # Prepare game details with the newly revealed box
                    rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
                    rendered_table = render_table(payoff_matrix, revealed, weights)
                    rendered_cost = render_cost(cost)

                    if nudge == "pre-supersize":
                        new_game = render(rendered_header, rendered_nudge, rendered_table, rendered_cost)
                    elif nudge == "post-supersize":
                        # If a basket has already been selected at least once, then show the nudge
                        if first_selected_basket is not None:
                            new_game = render(rendered_header, rendered_nudge, rendered_table, rendered_cost)
                        else:
                            rendered_table = render_table(payoff_matrix[:, :og_baskets], revealed, weights)
                            new_game = render(rendered_header, rendered_table, rendered_cost)
                    elif nudge == "control":
                        new_game = render(rendered_header, rendered_table, rendered_cost)

                elif action == "select":
                    logging.info("SELECT: {}".format(args))

                    # Calculate game results
                    points = payoff_matrix[:, int(args.get("basket"))-1]
                    total_points = np.sum(points * weights)
                    gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                    net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01

                    if nudge == "post-supersize":
                        # If selecting for the first time then show the nudge and continue the game;
                        # if selecting for the second time then show and include results, and finish the game
                        if first_selected_basket is None:
                            first_selected_basket = int(args.get("basket"))

                            # Prepare game details with the earnings from the selected basket
                            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
                            rendered_table = render_table(payoff_matrix, revealed, weights)
                            rendered_cost = render_cost(cost)
                            rendered_result = render_result(weights, points, total_points, net_earnings)

                            new_game = render(rendered_header, rendered_nudge, rendered_table, rendered_cost)

                            # Update tools to be able to select the new basket
                            tools = self.get_control_tools(
                                list(range(len(weights))), # prize indices
                                list(range(1, shown_baskets+1)) # basket indices
                            )
                        else:
                            selected_basket = int(args.get("basket"))

                            if not is_practice:
                                total_earnings += net_earnings
                            chose_nudge = (selected_basket == (nudge_index + 1))
                            # Reveal the whole basket since it was selected
                            revealed[:, (int(args.get("basket"))-1)] = True

                            # Prepare game details with the earnings from the selected basket
                            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
                            rendered_table = render_table(payoff_matrix, revealed, weights)
                            rendered_cost = render_cost(cost)
                            rendered_result = render_result(weights, points, total_points, net_earnings)

                            new_game = render(rendered_header, rendered_nudge, rendered_table, rendered_cost, rendered_result)
                    else:
                        # Update state
                        selected_basket = first_selected_basket = int(args.get("basket"))
                        if not is_practice:
                            total_earnings += net_earnings
                        chose_nudge = (selected_basket == (nudge_index + 1))
                        # Reveal the whole basket since it was selected
                        revealed[:, (int(args.get("basket"))-1)] = True

                        # Prepare game details with the earnings from the selected basket
                        rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
                        rendered_table = render_table(payoff_matrix, revealed, weights)
                        rendered_cost = render_cost(cost)
                        rendered_result = render_result(weights, points, total_points, net_earnings)

                        if nudge == "pre-supersize":
                            new_game = render(rendered_header, rendered_nudge, rendered_table, rendered_cost, rendered_result)
                        elif nudge == "control":
                            new_game = render(rendered_header, rendered_table, rendered_cost, rendered_result)

                args[action] = new_game
                logging.info(new_game)

                # Create a message containing the result of the function call
                tool_response, function_call_result_message = render_tool_call(tool_call, args)
                messages.append(tool_response)
                messages.append(function_call_result_message)

            # Save values
            if not fewshot_learning:
                results.append(
                    {
                        "is_practice": row.is_practice,
                        "cost": row.cost,
                        "payoff_matrix": row.payoff_matrix,
                        "weights": row.weights,
                        "trial_num": row.trial_num,
                        "trial_nudge": row.trial_nudge,
                        "nudge_index": row.nudge_index,
                        "nudge_type": row.nudge_type,
                        "og_baskets": row.og_baskets,
                        "shown_baskets": row.shown_baskets,
                        "participant_id": row.participant_id,
                        "first_selected_option": first_selected_basket-1,
                        "selected_option": selected_basket-1,
                        "gross_earnings": gross_earnings,
                        "net_earnings": net_earnings,
                        "chose_nudge": chose_nudge,
                        "uncovered_values": uncovered_values
                     }
                )
        return results, messages

class Highlight(MultiAttribute):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def get_fewshot_data(
            self,
            participants: list,
            n_examples: int
    ):
        # Remove participants already selected and practice trials
        df = self.data[~self.data.participant_id.isin(participants)
                       & ~self.data.is_practice]
        # Shuffle and drop duplicates
        df = df.sample(
            frac=1,
            random_state=self.seed
        ).drop_duplicates(['weights'])
        # Select control trials (i.e. no highlight)
        df_control = df[df.is_control
                        & (df.uncovered_values != "[]")].sample(
            n=n_examples // 2,
            random_state=self.seed
        )
        # Select control trials (i.e. no highlight)
        df_highlight = df[~df.is_control
                          & (df.uncovered_values != "[]")].sample(
            n=n_examples // 2,
            random_state=self.seed
        )
        # Final dataframe of examples
        return pd.concat([df_control,
                          df_highlight])

    def render_nudge(self, costs):
        # e.g. Cost of revealing prize A=3 points, B=1 point, and C=3 points
        prizes = [chr(i+65) for i in range(len(costs))] # prize letters
        costs_str = [f"{p}={c} points" if c > 1 else f"{p}={c} point" for p, c in zip(prizes, costs)]
        nudge = "Cost of revealing prize " + ", ".join(costs_str[:-1]) + ", and " + costs_str[-1]
        return nudge

    def run_trials(self, df, initial_messages, is_practice, fewshot_learning):
        # Global state
        total_earnings = 1.3 # dollars

        # Iterate through all (practice and test) games
        results = []
        for idx, row in enumerate(df.itertuples()):
            if (not fewshot_learning) or idx==0:
                # For regular games, messages only in the context
                # of the current game. For few-shot learning, all messages.
                messages = initial_messages.copy()

            # Game details
            payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
            revealed = np.full(payoff_matrix.shape, False)
            weights = np.array(ast.literal_eval(row.weights))
            nudge_index = int(row.highlight_index)
            original_cost_matrix = np.array(ast.literal_eval(row.original_cost_matrix))
            reveal_cost_array = np.apply_along_axis(np.max, axis=1, arr=original_cost_matrix)
            n_total_trials = df.shape[0]
            n_trial = (idx % n_total_trials) + 1
            human_uncovered_values = ast.literal_eval(row.uncovered_values)

            # Game state
            selected_basket = None
            uncovered_values = []
            cost = 0
            net_earnings = 0
            gross_earnings = 0

            # Format initial game
            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
            rendered_nudge = self.render_nudge(reveal_cost_array)
            rendered_table = render_table(payoff_matrix, revealed, weights)
            rendered_cost = render_cost(cost)

            game = render(
                rendered_header,
                rendered_nudge,
                rendered_table,
                rendered_cost
            )
            tools = self.get_control_tools(
                list(range(len(weights))), # prize indices
                list(range(1, payoff_matrix.shape[1]+1)) # basket indices
            )

            # Start interaction with initial game
            logging.info(game)
            messages.append({"role": "user", "content": game})

            # Interact until the end of game (i.e. basket is selected)
            while selected_basket is None:
                if not fewshot_learning:
                    response = self.api_call(messages=messages, tools=tools)

                    # Parse response
                    tool_call = response.choices[0].message.tool_calls[0]
                    action = tool_call.function.name
                    args = json.loads(tool_call.function.arguments)
                else:
                    # If few shot learning, ignore responses and simulate it
                    if human_uncovered_values:
                        action = "reveal"
                        flat_index = human_uncovered_values.pop(0)
                        args = {"prize": chr((flat_index // payoff_matrix.shape[1]) + 65),
                                "basket": flat_index % payoff_matrix.shape[1] + 1}
                    else:
                        action = "select"
                        args = {"basket": row.selected_option+1}

                    tool_call = ChatCompletionMessageToolCall(
                        id="call_" + "".join([random.choice(string.ascii_letters + string.digits) for _ in range(24)]),
                        type="function",
                        function=Function(
                            name=action, arguments=json.dumps(args)
                        )
                    )

                if action == "reveal":
                    logging.info("REVEAL: {}".format(args))

                    # Update state
                    prize = ord(args.get("prize")) - 65 # Letter to index
                    basket_idx = int(args.get("basket")) - 1
                    uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                    revealed[prize, basket_idx] = True
                    cost += reveal_cost_array[prize]

                    # Prepare game details with the newly revealed box
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        rendered_nudge,
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost)
                    )

                elif action == "select":
                    logging.info("SELECT: {}".format(args))

                    # Calculate game results
                    points = payoff_matrix[:, int(args.get("basket"))-1]
                    total_points = np.sum(points * weights)
                    gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                    net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01

                    # Update state
                    selected_basket = int(args.get("basket"))
                    if not is_practice:
                        total_earnings += net_earnings
                    # Reveal the whole basket since it was selected
                    revealed[:, (int(args.get("basket"))-1)] = True

                    # Prepare game details with the earnings from the selected basket
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        rendered_nudge,
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost),
                        render_result(weights, points, total_points, net_earnings)
                    )

                args[action] = new_game
                logging.info(new_game)

                # Create a message containing the result of the function call
                tool_response, function_call_result_message = render_tool_call(tool_call, args)
                messages.append(tool_response)
                messages.append(function_call_result_message)

            # Save values
            if not fewshot_learning:
                results.append(
                    {
                        "is_practice": row.is_practice,
                        "is_control": row.is_control,
                        "cost_matrix": row.original_cost_matrix,
                        "payoff_matrix": row.payoff_matrix,
                        "weights": row.weights,
                        "trial_num": row.trial_num,
                        "nudge_index": row.highlight_index,
                        "trial_type": row.trial_type,
                        "participant_id": row.participant_id,
                        "selected_option": selected_basket-1,
                        "gross_earnings": gross_earnings,
                        "net_earnings": net_earnings,
                        "uncovered_values": uncovered_values
                     }
                )
        return results, messages


class Optimal(MultiAttribute):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def get_fewshot_data(
            self,
            participants: list,
            n_examples: int
    ):
        raise NotImplementedError("Few shot learning is not supported for this nudge")

    def run_trials(self, df, initial_messages, is_practice, fewshot_learning):
        # Global state
        total_earnings = 1.3 # dollars

        # Iterate through all (practice and test) games
        results = []
        for idx, row in enumerate(df.itertuples()):
            if (not fewshot_learning) or idx==0:
                # For regular games, messages only in the context
                # of the current game. For few-shot learning, all messages.
                messages = initial_messages.copy()

            # Game details
            payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
            cost_matrix = np.array(ast.literal_eval(row.cost_matrix))
            revealed = (cost_matrix == 0) # reveal initial cells
            weights = np.array(ast.literal_eval(row.weights))
            reveal_cost = int(np.array(ast.literal_eval(row.cost_matrix)).max())
            n_total_trials = df.shape[0]
            n_trial = (idx % n_total_trials) + 1

            # Game state
            selected_basket = None
            uncovered_values = []
            cost = 0
            net_earnings = 0
            gross_earnings = 0

            # Format initial game
            rendered_header = render_header(total_earnings, is_practice, n_trial, n_total_trials)
            rendered_table = render_table(payoff_matrix, revealed, weights)
            rendered_cost = render_cost(cost)

            game = render(
                rendered_header,
                rendered_table,
                rendered_cost
            )
            tools = self.get_control_tools(
                list(range(len(weights))), # prize indices
                list(range(1, payoff_matrix.shape[1]+1)) # basket indices
            )

            # Start interaction with initial game
            logging.info(game)
            messages.append({"role": "user", "content": game})

            # Interact until the end of game (i.e. basket is selected)
            while selected_basket is None:
                response = self.api_call(messages=messages, tools=tools)

                # Parse response
                tool_call = response.choices[0].message.tool_calls[0]
                action = tool_call.function.name
                args = json.loads(tool_call.function.arguments)

                if action == "reveal":
                    logging.info("REVEAL: {}".format(args))

                    # Update state
                    prize = ord(args.get("prize")) - 65 # Letter to index
                    basket_idx = int(args.get("basket")) - 1
                    uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                    revealed[prize, basket_idx] = True
                    cost += reveal_cost

                    # Prepare game details with the newly revealed box
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost)
                    )

                elif action == "select":
                    logging.info("SELECT: {}".format(args))

                    # Calculate game results
                    points = payoff_matrix[:, int(args.get("basket"))-1]
                    total_points = np.sum(points * weights)
                    gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                    net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01

                    # Update state
                    selected_basket = int(args.get("basket"))
                    if not is_practice:
                        total_earnings += net_earnings
                    # Reveal the whole basket since it was selected
                    revealed[:, (int(args.get("basket"))-1)] = True

                    # Prepare game details with the earnings from the selected basket
                    new_game = render(
                        render_header(
                            total_earnings,
                            is_practice,
                            n_trial,
                            n_total_trials
                        ),
                        render_table(payoff_matrix, revealed, weights),
                        render_cost(cost),
                        render_result(weights, points, total_points, net_earnings)
                    )

                args[action] = new_game
                logging.info(new_game)

                # Create a message containing the result of the function call
                tool_response, function_call_result_message = render_tool_call(tool_call, args)
                messages.append(tool_response)
                messages.append(function_call_result_message)

            # Save values
            if not fewshot_learning:
                results.append(
                    {
                        "is_practice": row.is_practice,
                        "payoff_matrix": row.payoff_matrix,
                        "cost_matrix": row.cost_matrix,
                        "weights": row.weights,
                        "trial_num": row.trial_num,
                        "selected_option": selected_basket-1,
                        "uncovered_values": uncovered_values,
                        "nudge_type": row.nudge_type,
                        "participant_id": row.participant_id,
                        "gross_earnings": gross_earnings,
                        "net_earnings": net_earnings
                     }
                )
        return results, messages
