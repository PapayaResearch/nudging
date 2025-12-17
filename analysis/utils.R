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

library(tidyverse)
library(fixest)
library(emmeans)
library(ggdist)
library(ggsci)
library(gridExtra)
library(grid)
library(patchwork)
library(kableExtra)
library(ggtext)
library(directlabels)
library(modelsummary)

emm_options(
  lmerTest.limit = 100000,
  pbkrtest.limit = 100000
)
options(
  modelsummary_format_numeric_latex = "plain",
  modelsummary_factory_latex = "kableExtra"
)

# ============================================================================
# CONSTANTS
# ============================================================================

RANDOM_PAYOFF <- 150
MAXIMUM_PAYOFF <- 183.63861
SOURCE_LEVELS <- c(
  "Human",
  "GPT-3.5 Turbo",
  "GPT-4o Mini",
  "GPT-4o",
  "Gemini 1.5 Flash",
  "Gemini 1.5 Pro",
  "Gemini 2.5 Flash",
  "Gemini 2.5 Pro",
  "Gemini 2.5 Pro-Min",
  "Gemini 2.5 Pro-Med",
  "Claude 3 Haiku",
  "Claude 3.5 Sonnet",
  "Claude 4.5 Sonnet",
  "Claude 4.5 Sonnet-Low",
  "Claude 4.5 Sonnet-Med",
  "o3 Mini",
  "o3",
  "GPT-5",
  "GPT-5R-Min",
  "GPT-5R-Low",
  "GPT-5R-Med",
  "GPT-5 Mini"
)

source_mapping <- c(
  "real" = "Human",
  "gpt-3.5-turbo-0125" = "GPT-3.5 Turbo",
  "gpt-4o-mini-2024-07-18" = "GPT-4o Mini",
  "gemini/gemini-1.5-flash" = "Gemini 1.5 Flash",
  "gemini/gemini-1.5-pro" = "Gemini 1.5 Pro",
  "gemini/gemini-2.5-flash" = "Gemini 2.5 Flash",
  "gemini/gemini-2.5-pro" = "Gemini 2.5 Pro",
  "gemini/gemini-2.5-pro_minimal" = "Gemini 2.5 Pro-Min",
  "gemini/gemini-2.5-pro_medium" = "Gemini 2.5 Pro-Med",
  "claude-3-haiku-20240307" = "Claude 3 Haiku",
  "claude-3-5-sonnet-20241022" = "Claude 3.5 Sonnet",
  "claude-sonnet-4-5-20250929" = "Claude 4.5 Sonnet",
  "claude-sonnet-4-5-20250929_low" = "Claude 4.5 Sonnet-Low",
  "claude-sonnet-4-5-20250929_medium" = "Claude 4.5 Sonnet-Med",
  "gpt-4o-2024-08-06" = "GPT-4o",
  "o3-mini" = "o3 Mini",
  "o3-2025-04-16" = "o3",
  "gpt-5-2025-08-07" = "GPT-5",
  "gpt-5-2025-08-07_minimal" = "GPT-5R-Min",
  "gpt-5-2025-08-07_low" = "GPT-5R-Low",
  "gpt-5-2025-08-07_medium" = "GPT-5R-Med",
  "gpt-5-mini-2025-08-07" = "GPT-5 Mini"
)

colors_prompt <- c("#800000FF", "#8A9045FF", "#FFA319FF")

scale_color_prompt <- function(...) {
  scale_color_manual(values = colors_prompt, ...)
}

scale_fill_prompt <- function(...) {
  scale_fill_manual(values = colors_prompt, ...)
}

theme_nudge <- function() {
  theme_light() +
    theme(
      axis.line.x.bottom = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank(),
      legend.title = element_text(face = "italic"),
      legend.text = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
}

# ============================================================================
# DATA PREPROCESSING
# ============================================================================

preprocess_data <- function(data, nudge_type = NULL, reasoning_evals = FALSE) {

  sources_for_reasoning_evals <- c(
    "gpt-5-2025-08-07_minimal",
    "gpt-5-2025-08-07_low",
    "gpt-5-2025-08-07_medium",
    "gemini/gemini-2.5-pro_minimal",
    "gemini/gemini-2.5-pro_medium",
    "claude-sonnet-4-5-20250929_low",
    "claude-sonnet-4-5-20250929_medium",
    "real"
  )

  processed <- data %>%
    subset(
      if (reasoning_evals) {
        source %in% sources_for_reasoning_evals
      } else {
        !(source %in% sources_for_reasoning_evals) | (source == "real")
      }
    ) %>%
    mutate(
      n_prizes = factor(n_prizes),
      n_baskets = factor(n_baskets),
      grid_shape = interaction(n_baskets, n_prizes, sep = "x"),
      is_practice = is_practice %>% recode(
        "True" = TRUE,
        "False" = FALSE
      ),

      total_points = net_earnings * 3000,

      Features = n_prizes,
      Options = n_baskets,

      source = source %>% recode(!!!source_mapping) %>%
        factor(levels = SOURCE_LEVELS),

      method = ifelse(cot, "CoT", ifelse(fs > 0, "FS", "Base"))
    )

  processed <- processed %>%
    mutate(
      source_parent = case_when(
        source == "Human" ~ " ",
        str_detect(as.character(source), "GPT") ~ "GPT",
        str_detect(as.character(source), "Gemini") ~ "Gemini",
        str_detect(as.character(source), "Claude") ~ "Claude",
        TRUE ~ "  "
      ),
      source_child = case_when(
        source == " " ~ "Human",
        source == "GPT-3.5 Turbo" ~ "3.5 Turbo",
        source == "GPT-4o Mini" ~ "4o Mini",
        source == "GPT-4o" ~ "4o",
        source == "GPT-5 Mini" ~ "5 Mini",
        source == "GPT-5" ~ "5",
        source == "GPT-5R-Min" ~ "5R-Min",
        source == "GPT-5R-Low" ~ "5R-Low",
        source == "GPT-5R-Med" ~ "5R-Med",
        source == "Gemini 1.5 Flash" ~ "1.5 Flash",
        source == "Gemini 1.5 Pro" ~ "1.5 Pro",
        source == "Gemini 2.5 Flash" ~ "1.5 Flash",
        source == "Gemini 2.5 Pro" ~ "2.5 Pro",
        source == "Gemini 2.5 Pro-Min" ~ "2.5 Pro-Min",
        source == "Gemini 2.5 Pro-Med" ~ "2.5 Pro-Med",
        source == "Claude 3 Haiku" ~ "3 Haiku",
        source == "Claude 3.5 Sonnet" ~ "3.5 Sonnet",
        source == "Claude 4.5 Sonnet" ~ "4.5 Sonnet",
        source == "Claude 4.5 Sonnet-Low" ~ "4.5 Sonnet-Low",
        source == "Claude 4.5 Sonnet-Med" ~ "4.5 Sonnet-Med",
        source == "o3 Mini" ~ "o3 Mini",
        source == "o3" ~ "o3",
        TRUE ~ as.character(source)
      )
    )

  PARENT_LEVELS_ORDER <- c(" ", "GPT", "Gemini", "Claude", "  ")
  CHILD_LEVELS_ORDER <- c(
    "Human",
    "3.5 Turbo", "4o Mini", "4o", "5", "5 Mini", "5R-Min", "5R-Low", "5R-Med",
    "1.5 Flash", "1.5 Pro", "2.5 Flash", "2.5 Pro", "2.5 Pro-Min", "2.5 Pro-Med",
    "3 Haiku", "3.5 Sonnet", "4.5 Sonnet", "4.5 Sonnet-Low", "4.5 Sonnet-Med",
    "o3 Mini", "o3"
  )

  processed <- processed %>%
    mutate(
      source_parent = factor(source_parent, levels = PARENT_LEVELS_ORDER),
      source_child = factor(source_child, levels = CHILD_LEVELS_ORDER)
    )

  processed <- processed %>% subset(!is_practice)

  if (!is.null(nudge_type)) {

    if (nudge_type == "default") {
      processed <- processed %>% mutate(
        trial_nudge = trial_nudge %>% recode(
          "control" = "Abs.",
          "default" = "Pres."
        ),
        is_nudge_index_optimal = nudge_index == optimal_option
      )
    } else if (nudge_type == "highlight") {
      processed <- processed %>% mutate(
        trial_nudge = is_control %>% recode(
          "True" = "Abs.",
          "False" = "Pres."
        ),
        highlight_revealsn = highlight_reveals / (n_uncovered + 1e-8),
        weights = weights %>% str_extract_all("\\d+(\\.\\d+)?") %>% map(as.numeric),
        highest_prize = (weights %>% map_dbl(which.max)) - 1,
        is_nudge_index_optimal = ifelse(nudge_index == highest_prize, "Optimal", "Suboptimal") %>% factor(levels = c("Optimal", "Suboptimal")),
        is_first_index_nudged = is_first_index_nudged %>% recode(
          "True" = 1,
          "False" = 0
        )
      )
    } else if (nudge_type == "suggestion") {
      processed <- processed %>% mutate(
        trial_nudge = trial_nudge %>% recode(
          "control" = "Abs.",
          "pre-supersize" = "Early",
          "post-supersize" = "Late"
        ) %>% factor(levels = c("Abs.", "Early", "Late")),
        should_change = value_first_option_selected < value_final_option_selected
      )
    } else if (nudge_type == "optimal") {
      processed <- processed %>% mutate(
        nudge_type = nudge_type %>% recode(
          "random" = "Random",
          "extreme" = "Extreme",
          "greedy" = "Optimal"
        ) %>% factor(levels = c("Random", "Extreme", "Optimal"))
      )
    }
  }

  processed %>% droplevels()
}

sep_model_and_reasoning <- function(data) {
  data %>%
    mutate(
      model = source %>% recode(
        "Gemini 2.5 Pro-Min" = "Gemini 2.5 Pro",
        "Gemini 2.5 Pro-Med" = "Gemini 2.5 Pro",
        "GPT-5R-Min" = "GPT-5",
        "GPT-5R-Low" = "GPT-5",
        "GPT-5R-Med" = "GPT-5",
        "Claude 4.5 Sonnet-Low" = "Claude 4.5 Sonnet",
        "Claude 4.5 Sonnet-Med" = "Claude 4.5 Sonnet",
        "Human" = "Human"
      ) %>% factor(
        levels = c("Gemini 2.5 Pro", "GPT-5", "Claude 4.5 Sonnet", "Human")
      ),
      reasoning_effort = source %>% recode(
        "Gemini 2.5 Pro-Min" = "Low",
        "Gemini 2.5 Pro-Med" = "Medium",
        "GPT-5R-Min" = "Minimal",
        "GPT-5R-Low" = "Low",
        "GPT-5R-Med" = "Medium",
        "Claude 4.5 Sonnet-Low" = "Low",
        "Claude 4.5 Sonnet-Med" = "Medium",
        "Human" = "Unknown"
      ) %>% factor(
        levels = c("Minimal", "Low", "Medium", "Unknown")
      )
    )
}

# ============================================================================
# ANALYSIS UTILITIES
# ============================================================================

calculate_ks_stats <- function(
  data,
  filter_condition = NULL,
  group_vars = c("source", "method")
) {
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))
  }

  human_data <- data %>%
    filter(source == "Human") %>%
    pull(n_uncovered)

  result <- data %>%
    filter(source != "Human") %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      ks_stat = ks.test(
        n_uncovered,
        human_data
      )$statistic,
      ks_p = ks.test(
        n_uncovered,
        human_data
      )$p.value,
      .groups = "drop"
    ) %>%
    arrange(ks_stat) %>%
    as.data.frame() %>%
    mutate(
      ks_p = p.adjust(ks_p, method = "BH")
    )

  result
}

fit_reasoning_model <- function(data, outcome.var = "mean_reasoning_tokens") {
  data %>%
    subset(model != "Human") %>%
    droplevels() %>%
    feols(
      as.formula(sprintf("%s ~ model * reasoning_effort * trial_nudge", outcome.var)),
      data = .,
      vcov = cluster ~ participant_id
    )
}

analyze_earnings <- function(data, condition_var = "trial_nudge") {
  formula_str <- sprintf("total_points ~ source * %s | method", condition_var)
  model <- feols(as.formula(formula_str), data = data, vcov = cluster ~ participant_id)
  emm <- emmeans(model, as.formula(sprintf("~ source | %s", condition_var)), data = data)
  return(list(model = model, emm = emm))
}


# ============================================================================
# OUTPUT UTILITIES
# ============================================================================

make.regression_table <- function(..., output.path = NULL) {
  modelsummary(
    list(...),
    stars = c(`****` = 0.0001, `***` = 0.001, `**` = 0.01, `*` = 0.05),
    gof_omit = "AIC|BIC|Log.Lik.|Deviance|Num.Obs.",
    output = "latex_tabular",
    coef_rename = TRUE,
    estimate = "{estimate}{stars} ({std.error})",
    statistic = NULL
  ) %>%
    write_lines(output.path)
}

make.emm_table <- function(
  emm_data,
  names_from,
  values_select,
  caption,
  output.path,
  id_cols = c("model", "reasoning_effort"),
  rename_cols = c(Model = "model", `Reasoning Effort` = "reasoning_effort"),
  estimate_col = "prob",
  label = "tab:emm_table",
  strip_p_under_separation = TRUE
) {
  emm_table <- emm_data %>%
    add_significance_stars(p_col = "contrast_p_value") %>%
    mutate(
      EMM = paste0(round(!!sym(estimate_col), 3), "$^{", sig_stars, "}$", " (", round(SE, 3), ")"),
      EMM = ifelse(
        strip_p_under_separation & (abs(!!sym(estimate_col) - 1.0) < 1e-6),
        paste0(round(!!sym(estimate_col), 3), "$^{\\dagger}$ (", round(SE, 3), ")"),
        EMM
      )
    )
  
  emm_table %>%
    kbl("markdown") %>%
    write_lines(output.path %>% str_replace("\\.tex$", ".md"))
  
  emm_table %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = {{ names_from }},
      values_from = "EMM"
    ) %>%
    select(all_of(id_cols), all_of(values_select)) %>%
    rename(!!!rename_cols) %>%
    kbl(
      "latex",
      booktabs = TRUE,
      linesep = "",
      caption = caption,
      escape = FALSE,
      position = "!htb",
      label = label
    ) %>%
    write_lines(output.path)
}


save_table <- function(data, filename, digits = 4) {
  data %>%
    kbl(digits = digits) %>%
    cat(file = filename, sep = "\n", append = FALSE)
}

ensure_dirs <- function(dirs = c("figures", "tables", "results")) {
  for (dir in dirs) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
}

python_list_to_mean <- function(py_list_str) {
  py_list_str %>%
    str_remove_all("\\[|\\]") %>%
    str_split(", ") %>%
    map_dbl(~ mean(as.numeric(.x)) )
}

get_cost_per_token_for_model <- function(model_name, token_type = "input") {
  cost_mapping_1m_tokens <- case_when(
    token_type == "input" ~ list(
      "Human" = 0,
      "GPT-5" = 1.25,
      "Gemini 2.5 Pro" = 1.25,
      "Claude 4.5 Sonnet" = 3
    ),
    token_type == "output" ~ list(
      "Human" = 0,
      "GPT-5" = 10,
      "Gemini 2.5 Pro" = 10,
      "Claude 4.5 Sonnet" = 15
    )
  )
  
  model_name %>%
    sapply(function(name) {
      cost_mapping_1m_tokens[[name]] / 1e6
    })
}

