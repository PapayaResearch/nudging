# Copyright (c) 2024
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

import openai
import pandas as pd
import numpy as np
import exp1
import ast
import json
import argparse
import logging
import os
import tiktoken
from datetime import datetime
from tqdm.auto import tqdm

MAX_PARTICIPANTS = 1
TEMPERATURE = 0.2 # Default in the OpenAI API
ENC = tiktoken.encoding_for_model("gpt-4o")

def format_game(
        payoff_matrix,
        revealed,
        weights,
        cost,
        total_earnings,
        is_practice,
        n_game,
        n_game_total
):
    n_baskets = payoff_matrix.shape[1]
    header = ["Prizes"] + [f"Basket {i+1}" for i in range(n_baskets)]
    matrix = [header]
    for idx, (row, row_idx) in enumerate(zip(payoff_matrix, revealed)):
        matrix.append(
            [chr(idx + 65) + ": " + str(weights[idx]) + " points"] + [str(value)
                                                                      if flag else "?"
                                                                      for value, flag in zip(row, row_idx)]
        )

    markdown = pd.DataFrame(
        matrix[1:], columns=matrix[0]
    ).to_markdown(
        index=False,
        numalign="right",
        colalign=("right",)
    )

    game = f"Practice game {n_game} of {n_game_total}\n" if is_practice else f"Test game {n_game} of {n_game_total}\n"
    game += f"Total Earnings: ${total_earnings:.3f}\n"
    game += markdown
    # "Total reveal cost" was understood as cost to reveal
    game += f"\nTotal accumulated cost: {cost} points"
    return game

def get_game_tools(prizes, baskets):
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
                        "prize_points": {
                            "type": "array",
                            "items": {
                                "type": "integer"
                            },
                            "description": "List of points for each prize.",
                        },
                        "reason": {
                            "type": "string",
                            "description": "Explain your reasoning, and the value of the selected prize.",
                        },
                        "prize": {
                            "type": "string",
                            "enum": prizes,
                            "description": "The prize's letter corresponding to the box.",
                        },
                        "basket": {
                            "type": "integer",
                            "enum": baskets,
                            "description": "The basket's number corresponding to the box.",
                        },
                    },
                    "required": ["prize_points", "reason", "prize", "basket"],
                    "additionalProperties": False,
                },
            }
        },
        {
            "type": "function",
            "function": {
                "name": "select",
                "strict": True,
                "description": "Call this whenever you choose to select a basket. Remember prizes apply to all baskets equally. The total value of the selected basket is calculated as the dot product of prize points by its corresponding box points (if revealed).",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "prize_points": {
                            "type": "array",
                            "items": {
                                "type": "integer"
                            },
                            "description": "List of points for each prize.",
                        },
                        "box_points": {
                            "type": "string",
                            "description": "List of box points only for the selected basket.",
                        },
                        "dot_product": {
                            "type": "string",
                            "description": "Dot product of prize points by box points of the selected basket.",
                        },
                        "reason": {
                            "type": "string",
                            "description": "Explain your reasoning.",
                        },
                        "basket": {
                            "type": "integer",
                            "enum": baskets,
                            "description": "The basket's number.",
                        },
                    },
                    "required": ["prize_points", "box_points", "dot_product", "reason", "basket"],
                    "additionalProperties": False,
                },
            }
        }
    ]

def get_nudge_tools():
    return [
        {
            "type": "function",
            "function": {
                "name": "default",
                "strict": True,
                "description": "Call this to accept or decline the default basket. Take into account the point difference between the largest and smallest prize. You should not accept if the point difference is large.",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "point_difference": {
                            "type": "integer",
                            "description": "Point difference between the largest and smallest prize.",
                        },
                        "reason": {
                            "type": "string",
                            "description": "Explain your reasoning, mentioning the point difference and its magnitude.",
                        },
                        "decision": {
                            "type": "boolean",
                            "description": "Accept or decline the default basket.",
                        },
                    },
                    "required": ["point_difference", "reason", "decision"],
                    "additionalProperties": False,
                },
            }
        }
    ]

def get_quiz_tools():
    return [
        {
            "type": "function",
            "function": {
                "name": "quiz",
                "strict": True,
                "description": "Call this to answer the quiz.",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "question_1": {
                            "type": "integer",
                            "enum": [0, 1, 2],
                            "description": "Answer for question 1 with the right choice's index.",
                        },
                        "question_2": {
                            "type": "integer",
                            "enum": [0, 1, 2],
                            "description": "Answer for question 2 with the right choice's index.",
                        },
                        "question_3": {
                            "type": "integer",
                            "enum": [0, 1, 2, 3],
                            "description": "Answer for question 3 with the right choice's index.",
                        },
                        "question_4": {
                            "type": "integer",
                            "enum": [0, 1, 2],
                            "description": "Answer for question 4 with the right choice's index.",
                        },
                        "question_5": {
                            "type": "integer",
                            "enum": [0, 1, 2],
                            "description": "Answer for question 5 with the right choice's index.",
                        },
                    },
                    "required": ["question_1", "question_2", "question_3", "question_4", "question_5"],
                    "additionalProperties": False,
                },
            }
        }
    ]

def num_tokens_from_functions(functions):
    """Return the number of tokens used by a list of functions."""
    num_tokens = 0
    for function in functions:
        function_tokens = len(ENC.encode(function['name']))
        function_tokens += len(ENC.encode(function['description']))

        if 'parameters' in function:
            parameters = function['parameters']
            if 'properties' in parameters:
                for propertiesKey in parameters['properties']:
                    function_tokens += len(ENC.encode(propertiesKey))
                    v = parameters['properties'][propertiesKey]
                    for field in v:
                        if field == 'type':
                            function_tokens += 2
                            function_tokens += len(ENC.encode(v['type']))
                        elif field == 'description':
                            function_tokens += 2
                            function_tokens += len(ENC.encode(v['description']))
                        else:
                            print(f"Warning: not supported field {field}")
                function_tokens += 11

        num_tokens += function_tokens

    num_tokens += 12
    return num_tokens

def roll_games(df, initial_messages, model, temperature, is_practice):
    # Global state
    total_earnings = 1.3 # dollars

    # Iterate through all (practice and test) games
    results = []
    for idx, row in enumerate(df.itertuples()):
        # Messages only in the context of the current game
        messages = initial_messages.copy()
        # Game details
        payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
        revealed = np.full(payoff_matrix.shape, False)
        weights = np.array(ast.literal_eval(row.weights))
        nudge = row.trial_nudge == "default"
        nudge_index = int(row.nudge_index)
        default_cost = row.cost
        n_game_total = df.shape[0]
        n_game = (idx % n_game_total) + 1

        # Game state
        selected_basket = None
        uncovered_values = []
        cost = 0
        net_earnings = 0
        gross_earnings = 0
        accepted_default = False
        chose_nudge = False

        # Format initial game
        game = format_game(
            payoff_matrix,
            revealed,
            weights,
            cost,
            total_earnings,
            is_practice,
            n_game,
            n_game_total
        )
        if nudge:
            game = \
                f"\nDo you want to choose basket {nudge_index + 1}?" + \
                f"\nIt's pays the most when the prizes are equally valuable.\n" + \
                game
            tools = get_nudge_tools()
        else:
            tools = get_game_tools(
                list(range(len(weights))), # prize indices
                list(range(1, payoff_matrix.shape[1]+1)) # basket indices
            )
            # TODO: Remove print
            # print(num_tokens_from_functions([t.get("function") for t in tools]))

        # Start interaction with initial game
        logging.info(game)
        messages.append({"role": "user", "content": game})

        # Interact until the end of game (i.e. basket is selected)
        while selected_basket is None:
            # Note that if you force the model to call a function
            # then the subsequent finish_reason will be "stop" instead of being "tool_calls".
            # TODO: Remove print
            # print(sum([len(ENC.encode(m.get("content", ""))) for m in messages]))
            response = openai.chat.completions.create(
                model=model,
                messages=messages,
                tools=tools,
                parallel_tool_calls=False,
                tool_choice="required", # Force model to use one or more tools
                temperature=temperature, # Range [0, 2]
                max_tokens=300,
            )

            # Parse response
            tool_call = response.choices[0].message.tool_calls[0]
            action = tool_call.function.name
            args = json.loads(tool_call.function.arguments)

            if action == "reveal":
                logging.info("REVEAL: {}".format(args))

                # Update state
                prize = ord(args.get("prize")) - 65 # Letter to index
                basket_idx = args.get("basket") - 1
                uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                revealed[prize, basket_idx] = True
                cost += default_cost

                # Prepare game details with the newly revealed box
                new_game = format_game(
                    payoff_matrix,
                    revealed,
                    weights,
                    cost,
                    total_earnings,
                    is_practice,
                    n_game,
                    n_game_total
                )
                args["reveal"] = new_game

            elif action == "select":
                logging.info("SELECT: {}".format(args))

                # Calculate game results
                points = payoff_matrix[:, args.get("basket")-1]
                total_points = np.sum(points * weights)
                gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01
                prizes_list = [f"{v} {chr(k + 65)} prizes"
                               for k,v in zip(range(len(weights)), points)]
                prizes = ", ".join(prizes_list[:-1]) + ", and " + prizes_list[-1]

                # Update state
                selected_basket = args.get("basket")
                if not is_practice:
                    total_earnings += net_earnings
                chose_nudge = (selected_basket == (nudge_index + 1))

                # Prepare game details with the earnings from the selected basket
                new_game = format_game(
                    payoff_matrix,
                    revealed,
                    weights,
                    cost,
                    total_earnings,
                    is_practice,
                    n_game,
                    n_game_total
                )
                new_game += f"\nYou won {prizes}, totaling {total_points} points."
                new_game += f"\nTotal earnings (prize values minus reveal cost): ${net_earnings:.3f}"
                args["select"] = new_game

            elif action == "default":
                logging.info("DEFAULT: {}".format(args))

                if args.get("decision"):
                    # Calculate game results
                    points = payoff_matrix[:, nudge_index]
                    total_points = np.sum(points * weights)
                    gross_earnings = total_points * 0.00033333333 # 30 points = $0.01
                    net_earnings = (total_points - cost) * 0.00033333333 # 30 points = $0.01
                    prizes_list = [f"{v} {chr(k + 65)} prizes"
                                   for k,v in zip(range(len(weights)), points)]
                    prizes = ", ".join(prizes_list[:-1]) + ", and " + prizes_list[-1]

                    # Update state
                    selected_basket = nudge_index + 1
                    if not is_practice:
                        total_earnings += net_earnings
                    chose_nudge = True
                    accepted_default = True

                    # Prepare game details with the earnings from the selected basket
                    new_game = format_game(
                        payoff_matrix,
                        revealed,
                        weights,
                        cost,
                        total_earnings,
                        is_practice,
                        n_game,
                        n_game_total
                    )
                    new_game += f"\nYou won {prizes}, totaling {total_points} points."
                    new_game += f"\nTotal earnings (prize values minus reveal cost): ${net_earnings:.3f}"
                    args["default"] = new_game


                else:
                    # Update state
                    tools = get_game_tools(
                        list(range(len(weights))), # prize indices
                        list(range(1, payoff_matrix.shape[1]+1)) # basket indices
                    )

                    # Prepare game details with the newly revealed box
                    new_game = format_game(
                        payoff_matrix,
                        revealed,
                        weights,
                        cost,
                        total_earnings,
                        is_practice,
                        n_game,
                        n_game_total
                    )
                    args["default"] = new_game

            logging.info(new_game)

            # Simulate the tool call response
            tool_response = {
                "role": "assistant",
                "tool_calls": [
                    {
                        "type": "function",
                        "function": tool_call.function,
                        "id": tool_call.id
                    }
                ]
            }

            # Create a message containing the result of the function call
            function_call_result_message = {
                "role": "tool",
                "content": json.dumps(args),
                "tool_call_id": tool_call.id
            }

            messages.append(tool_response)
            messages.append(function_call_result_message)

        # Save values
        results.append({"is_practice": row.is_practice,
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
    return results


def roll_quiz(messages, model, temperature):
    # Initialize state
    tools = get_quiz_tools()
    pass_quiz = False
    correct_answers = [1, 1, 3, 1, 2]

    while not pass_quiz:
        # Ask questions
        messages.append({"role": "user", "content": exp1.QUIZ_PROMPT})

        # Note that if you force the model to call a function
        # then the subsequent finish_reason will be "stop" instead of being "tool_calls".
        response = openai.chat.completions.create(
            model=model,
            messages=messages,
            tools=tools,
            parallel_tool_calls=False,
            tool_choice="required", # Force model to use one or more tools
            temperature=temperature, # Range [0, 2]
            max_tokens=300
        )

        # Parse response
        tool_call = response.choices[0].message.tool_calls[0]
        args = json.loads(tool_call.function.arguments)
        answers = [args.get(f"question_{i}") for i in range (1, 6)]

        if answers == correct_answers:
            logging.info("CORRECT QUIZ: {}".format(args))

            # Update state
            pass_quiz = True
            args["quiz"] = "You passed the quiz!"

            # Simulate the tool call response
            tool_response = {
                "role": "assistant",
                "tool_calls": [
                    {
                        "type": "function",
                        "function": tool_call.function,
                        "id": tool_call.id
                    }
                ]
            }

            # Create a message containing the result of the function call
            function_call_result_message = {
                "role": "tool",
                "content": json.dumps(args),
                "tool_call_id": tool_call.id
            }

            messages.append(tool_response)
            messages.append(function_call_result_message)
        else:
            logging.info("INCORRECT QUIZ: {}".format(args))

            # Update state
            args["quiz"] = "You didn't pass the quiz."

            # Simulate the tool call response
            tool_response = {
                "role": "assistant",
                "tool_calls": [
                    {
                        "type": "function",
                        "function": tool_call.function,
                        "id": tool_call.id
                    }
                ]
            }

            # Create a message containing the result of the function call
            function_call_result_message = {
                "role": "tool",
                "content": json.dumps(args),
                "tool_call_id": tool_call.id
            }

            messages.append(tool_response)
            messages.append(function_call_result_message)

            # Help message for the quiz
            messages.append({"role": "user", "content": exp1.INCORRECT_QUIZ_PROMPT})

    return messages

def main():
    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, default="gpt-4o-mini")
    parser.add_argument("--max-participants", type=int, default=MAX_PARTICIPANTS)
    parser.add_argument("--temperature", type=float, default=TEMPERATURE)
    args = parser.parse_args()

    # Create a folder for logs if it doesn't exist
    log_folder = "../logs"
    os.makedirs(log_folder, exist_ok=True)

    # Create a log file with a datetime-based name
    date = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    log_filename = date + ".log"
    log_filepath = os.path.join(log_folder, log_filename)

    # Set up logging configuration
    logging.basicConfig(
        filename=log_filepath,
        level=logging.INFO,  # You can change this to DEBUG, ERROR, etc.
        format="%(asctime)s - %(levelname)s - %(message)s"
    )

    # Result CSV file
    results_file = "../results/" + date + ".csv"

    # Set up OpenAI API key
    with open("../oai.txt") as infile:
        openai.api_key = infile.read().strip()

    # Load data
    df = pd.read_csv("../data/default_data_exclusion.csv")

    # Iterate over participants
    all_results = []
    for pid in tqdm(df.participant_id.unique()[:args.max_participants]):
        logging.info("PARTICIPANT: {}".format(pid))

        df_participant = df[df.participant_id == pid].sort_values(by="trial_num",
                                                                  ascending=True)

        messages = [
            # {"role": "system", "content": exp1.NUDGE},
            {"role": "system", "content": exp1.INITIAL_PROMPT},
        ]

        # Quiz
        messages = roll_quiz(messages, args.model, args.temperature)

        # Practice games
        messages.append({"role": "user", "content": exp1.PRACTICE_PROMPT})
        df_practice = df_participant[df_participant.is_practice]
        results = roll_games(
            df_practice,
            messages,
            args.model,
            args.temperature,
            is_practice=True
        )
        if not os.path.exists(results_file):
            pd.DataFrame(results).to_csv(results_file, mode='w', header=True, index=False)
        else:
            pd.DataFrame(results).to_csv(results_file, mode='a', header=False, index=False)

        # Test games
        messages.append({"role": "user", "content": exp1.TEST_PROMPT})
        df_test = df_participant[~df_participant.is_practice]
        results = roll_games(
            df_test,
            messages,
            args.model,
            args.temperature,
            is_practice=False
        )
        pd.DataFrame(results).to_csv(results_file, mode='a', header=False, index=False)

if __name__ == "__main__":
    main()
