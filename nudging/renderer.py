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
import json

def render(*args):
    return "\n".join(args)

def render_prize(idx, weight):
    # e.g. A: 2 points
    return chr(idx + 65) + ": " + str(weight) + " points"

def render_table(payoff_matrix, revealed, weights):
    # e.g. | Prizes       | Basket 1 | Basket 2 |
    #      |--------------|----------|----------|
    #      | A: 16 points | 8        | 2        |
    #      | B: 14 points | ?        | ?        |
    n_baskets = payoff_matrix.shape[1]
    matrix = [["Prizes"] + [f"Basket {i+1}" for i in range(n_baskets)]]
    for idx, (row, row_idx) in enumerate(zip(payoff_matrix, revealed)):
        matrix.append(
            [render_prize(idx, weights[idx])] + [str(value)
                                                 if flag else "?"
                                                 for value, flag in zip(row, row_idx)]
        )

    # Table in markdown format
    table = pd.DataFrame(
        matrix[1:], columns=matrix[0]
    ).to_markdown(
        index=False,
        numalign="right",
        colalign=("right",)
    )
    return table

def render_header(total_earnings, is_practice, n_trial, n_total_trials):
    # e.g. Practice game 1 of 2\nTotal earnings: $1.353
    if is_practice:
        header = f"Practice game {n_trial} of {n_total_trials}\n"
    else:
        header = f"Test game {n_trial} of {n_total_trials}\n"

    header += f"Total earnings: ${total_earnings:.3f}"
    return header

def render_cost(cost):
    # e.g. Total accumulated cost: 4 points
    return f"Total accumulated cost: {cost} points"

def render_result(weights, points, total_points, net_earnings):
    prizes_list = [f"{v} {chr(k + 65)} prizes"
                   for k,v in zip(range(len(weights)), points)]
    prizes = ", ".join(prizes_list[:-1]) + ", and " + prizes_list[-1]

    result = f"You won {prizes}, totaling {total_points} points."
    result += f"\nTotal earnings (prize values minus reveal cost): ${net_earnings:.3f}"
    return result

def render_tool_call(tool_call, args):
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
    return tool_response, function_call_result_message
