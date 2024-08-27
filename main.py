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
from tqdm.auto import tqdm

MAX_PARTICIPANTS = 1

def format_game(payoff_matrix, revealed, weights, cost, total_earnings):
    n_baskets = payoff_matrix.shape[1]
    header = ["Prize", "Points"] + [f"Basket {i+1}" for i in range(n_baskets)]
    matrix = [header]
    for idx, (row, row_idx) in enumerate(zip(payoff_matrix, revealed)):
        matrix.append(
            [chr(idx + 65), str(weights[idx])] + [str(value)
                                                  if flag else ""
                                                  for value, flag in zip(row, row_idx)]
        )

    game = f"Total Earnings: ${total_earnings:.3f}\n"
    game += "\n".join([",".join(l) for l in matrix])
    game += f"\nTotal reveal cost: {cost} points"
    return game

def get_game_tools(prizes, baskets):
    return [
        {
            "type": "function",
            "function": {
                "name": "reveal",
                "strict": True,
                "description": "Call this whenever you need to reveal the value of a box",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "prize": {
                            "type": "integer",
                            "enum": prizes,
                            "description": "The prize's index corresponding to the box.",
                        },
                        "basket": {
                            "type": "integer",
                            "enum": baskets,
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
                "description": "Call this whenever you need to select a basket",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "basket": {
                            "type": "integer",
                            "enum": baskets,
                            "description": "The basket's index.",
                        },
                    },
                    "required": ["basket"],
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
                "name": "nudge",
                "strict": True,
                "description": "Call this whenever you need to accept or decline a default basket.",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "default": {
                            "type": "boolean",
                            "description": "Accept or decline the basket.",
                        },
                    },
                    "required": ["default"],
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
                "description": "Call this whenever you need to answer the quiz.",
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

def roll_games(df, messages, model, is_practice):
    # Global state
    total_earnings = 1.3 # dollars

    # Iterate through all (practice and test) games
    results = []
    for row in df.itertuples():
        # Game details
        payoff_matrix = np.array(ast.literal_eval(row.payoff_matrix))
        revealed = np.full(payoff_matrix.shape, False)
        weights = np.array(ast.literal_eval(row.weights))
        nudge = row.trial_nudge == "default"
        nudge_index = int(row.nudge_index)
        default_cost = row.cost

        # Game state
        selected_basket = None
        uncovered_values = []
        cost = 0
        net_earnings = 0
        gross_earnings = 0
        accepted_default = False
        chose_nudge = False

        # Format initial game
        game = format_game(payoff_matrix, revealed, weights, cost, total_earnings)
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

        # Start interaction with initial game
        print("\n")
        print(game)
        messages.append({"role": "user", "content": game})

        # Interact until the end of game (i.e. basket is selected)
        while selected_basket is None:
            # Note that if you force the model to call a function
            # then the subsequent finish_reason will be "stop" instead of being "tool_calls".
            response = openai.chat.completions.create(
                model=model,
                messages=messages,
                tools=tools,
                tool_choice="required" # Force model to use one or more tools
            )

            # Parse response
            tool_call = response.choices[0].message.tool_calls[0]
            action = tool_call.function.name
            args = json.loads(tool_call.function.arguments)

            if action == "reveal":
                print("\n")
                print("REVEAL: ", args)

                # Update state
                prize = args.get("prize")
                basket_idx = args.get("basket") - 1
                uncovered_values.append(prize * payoff_matrix.shape[1] + basket_idx)
                revealed[prize, basket_idx] = True
                cost += default_cost

                # Prepare game details with the newly revealed box
                new_game = format_game(payoff_matrix, revealed, weights, cost, total_earnings)
                args["reveal"] = new_game

            elif action == "select":
                print("\n")
                print("SELECT: ", args)

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
                accepted_default = (selected_basket == (nudge_index + 1))

                # Prepare game details with the earnings from the selected basket
                new_game = format_game(payoff_matrix, revealed, weights, cost, total_earnings)
                new_game += f"\nYou won {prizes}, totaling {total_points} points."
                new_game += f"\nTotal earnings (prize values minus reveal cost): ${net_earnings:.3f}"
                args["select"] = new_game

            elif action == "nudge":
                print("\n")
                print("NUDGE: ", args)

                if args.get("default"):
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
                    new_game = format_game(payoff_matrix, revealed, weights, cost, total_earnings)
                    new_game += f"\nYou won {prizes}, totaling {total_points} points."
                    new_game += f"\nTotal earnings (prize values minus reveal cost): ${net_earnings:.3f}"
                    args["nudge"] = new_game


                else:
                    # Update state
                    tools = get_game_tools(
                        list(range(len(weights))), # prize indices
                        list(range(1, payoff_matrix.shape[1]+1)) # basket indices
                    )

                    # Prepare game details with the newly revealed box
                    new_game = format_game(payoff_matrix, revealed, weights, cost, total_earnings)
                    args["nudge"] = new_game

            print("\n")
            print(new_game)

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
    return messages, results


def roll_quiz(messages, model):
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
            tool_choice="required" # Force model to use one or more tools
        )

        # Parse response
        tool_call = response.choices[0].message.tool_calls[0]
        args = json.loads(tool_call.function.arguments)
        answers = [args.get(f"question_{i}") for i in range (1, 6)]

        if answers == correct_answers:
            print("\n")
            print("CORRECT QUIZ: ", args)

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
            print("\n")
            print("INCORRECT QUIZ: ", args)

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
    args = parser.parse_args()

    # Set up OpenAI API key
    with open("oai.txt") as infile:
        openai.api_key = infile.read().strip()

    # Load data
    df = pd.read_csv("default_data.csv")

    # Iterate over participants
    all_results = []
    for pid in tqdm(df.participant_id.unique()[:args.max_participants]):
        print(f"Participant {pid}")

        df_participant = df[df.participant_id == pid].sort_values(by="trial_num",
                                                                  ascending=True)

        messages = [
            {"role": "system", "content": exp1.NUDGE},
            {"role": "user", "content": exp1.INITIAL_PROMPT},
        ]

        # Quiz
        messages = roll_quiz(messages, args.model)

        # Practice games
        messages.append({"role": "user", "content": exp1.PRACTICE_PROMPT})
        df_practice = df_participant[df_participant.is_practice]
        messages, results = roll_games(df_practice, messages, args.model, is_practice=True)
        all_results.extend(results)

        # Test games
        messages.append({"role": "user", "content": exp1.TEST_PROMPT})
        df_test = df_participant[~df_participant.is_practice]
        messages, results = roll_games(df_test, messages, args.model, is_practice=False)
        all_results.extend(results)

    pd.DataFrame(all_results).to_csv("results.csv", index=False)

if __name__ == "__main__":
    main()
