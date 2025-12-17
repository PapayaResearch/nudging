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

library(emmeans)
library(tidyverse)

get_marginal_effects <- function(model, specs, data = NULL) {
  is_logistic <- inherits(model, "fixest") &&
    !is.null(model$family) &&
    model$family$family == "binomial"

  emm <- if (is_logistic) {
    emmeans(model, specs = specs, type = "response", data = data)
  } else {
    emmeans(model, specs = specs, data = data)
  }

  emm[!(emm@grid$.wgt == 0)]
}

get_earnings_marginals <- function(model, formula_str) {
  if (grepl("trial_nudge", formula_str)) {
    emmeans(model, ~ source | trial_nudge)
  } else if (grepl("nudge_type", formula_str)) {
    emmeans(model, ~ source | nudge_type)
  } else {
    emmeans(model, ~ source)
  }
}

compute_human_contrasts <- function(emm_obj, by_vars = NULL, adjustment = "BH") {
  emm_obj %>%
    contrast(method = "trt.vs.ctrl", ref = 1, by = by_vars, adjust = adjustment) %>%
    summary(infer = TRUE) %>%
    as_tibble() %>%
    clean_contrast_labels()
}

create_human_contrasts <- function(emmeans_obj) {
  contrast(emmeans_obj, method = "trt.vs.ctrl", ref = 1, adjust = "BH")
}

compute_emmeans_pairwise <- function(
  model,
  data,
  source_vars = c("model", "reasoning_effort"),
  by = source_vars
) {
  formula_str <- paste0("~ trial_nudge | ", paste(source_vars, collapse = " + "))
  is_logistic <- inherits(model, "fixest") &&
    !is.null(model$family) &&
    model$family$family == "binomial"

  emm <- emmeans(model, as.formula(formula_str), data = data)

  wgt_col <- intersect(c(".wgt.", ".wgt"), names(emm@grid))[1]
  emm_filtered <- if (!is.null(wgt_col)) {
    emm[!(emm@grid[[wgt_col]] == 0)]
  } else {
    emm
  }

  emm_tibble <- if (is_logistic) {
    summary(emm_filtered, infer = TRUE, type = "response") %>% as_tibble()
  } else {
    summary(emm_filtered, infer = TRUE) %>% as_tibble()
  } %>% select(-any_of("p.value"))

  contrasts <- emm_filtered %>%
    contrast(method = "pairwise", by = by) %>%
    {if (is_logistic) summary(., infer = TRUE, type = "response") else summary(., infer = TRUE)} %>%
    as_tibble()

  estimate_col <- intersect(c("estimate", "odds.ratio"), names(contrasts))[1]
  contrasts <- contrasts %>%
    select(any_of(c(source_vars, "contrast", "p.value", estimate_col))) %>%
    rename(
      contrast_p_value = any_of("p.value"),
      contrast_estimate = any_of(estimate_col)
    )

  standardize_keys <- function(df, vars) {
    vars_in <- intersect(vars, names(df))
    if (length(vars_in) == 0) return(df)
    df %>%
      mutate(across(all_of(vars_in), ~ if (is.logical(.x) || is.factor(.x)) as.character(.x) else .x))
  }

  join_vars <- intersect(source_vars, intersect(names(emm_tibble), names(contrasts)))
  if ("trial_nudge" %in% names(emm_tibble) && "trial_nudge" %in% names(contrasts)) {
    join_vars <- c(join_vars, "trial_nudge")
  }

  emm_tibble %>%
    standardize_keys(join_vars) %>%
    left_join(standardize_keys(contrasts, join_vars), by = join_vars)
}

compute_emmeans_trt_vs_ctrl <- function(
  model,
  data,
  source_vars = c("model", "reasoning_effort"),
  by = source_vars
) {
  formula_str <- paste0("~ trial_nudge | ", paste(source_vars, collapse = " + "))
  is_logistic <- inherits(model, "fixest") &&
    !is.null(model$family) &&
    model$family$family == "binomial"

  emm <- emmeans(model, as.formula(formula_str), data = data)

  wgt_col <- intersect(c(".wgt.", ".wgt"), names(emm@grid))[1]
  emm_filtered <- if (!is.null(wgt_col)) {
    emm[!(emm@grid[[wgt_col]] == 0)]
  } else {
    emm
  }

  emm_tibble <- if (is_logistic) {
    summary(emm_filtered, infer = TRUE, type = "response") %>% as_tibble()
  } else {
    summary(emm_filtered, infer = TRUE) %>% as_tibble()
  } %>% select(-any_of("p.value"))

  contrasts <- emm_filtered %>%
    contrast(method = "trt.vs.ctrl", ref = 1, by = by) %>%
    {if (is_logistic) summary(., infer = TRUE, type = "response") else summary(., infer = TRUE)} %>%
    as_tibble()

  estimate_col <- intersect(c("estimate", "odds.ratio"), names(contrasts))[1]
  contrasts <- contrasts %>%
    select(any_of(c(source_vars, "contrast", "p.value", estimate_col))) %>%
    mutate(contrast = str_replace_all(contrast, " - Abs\\.", "")) %>%
    rename(
      trial_nudge = contrast,
      contrast_p_value = any_of("p.value"),
      contrast_estimate = any_of(estimate_col)
    )

  standardize_keys <- function(df, vars) {
    vars_in <- intersect(vars, names(df))
    if (length(vars_in) == 0) return(df)
    df %>%
      mutate(across(all_of(vars_in), ~ if (is.logical(.x) || is.factor(.x)) as.character(.x) else .x))
  }

  join_vars <- intersect(source_vars, intersect(names(emm_tibble), names(contrasts)))
  if ("trial_nudge" %in% names(emm_tibble) && "trial_nudge" %in% names(contrasts)) {
    join_vars <- c(join_vars, "trial_nudge")
  }

  emm_tibble %>%
    standardize_keys(join_vars) %>%
    left_join(standardize_keys(contrasts, join_vars), by = join_vars)
}

clean_contrast_labels <- function(contrast_df) {
  contrast_df %>%
    mutate(
      contrast = str_replace_all(contrast, "Abs\\.|Pres\\.", ""),
      contrast = str_replace(contrast, "Human Base", "Human"),
      method = str_extract(contrast, "Base|CoT|FS"),
      contrast = str_replace_all(contrast, "Base|CoT|FS", ""),
      contrast = str_replace_all(contrast, "\\(|\\)", ""),
      contrast = str_replace_all(contrast, "\\s+", " ") %>% str_trim()
    )
}

combine_earnings_data <- function(emm_obj, contrast_obj, exp_name) {
  emm_data <- emm_obj %>%
    as_tibble() %>%
    mutate(experiment = exp_name)

  contrast_data <- contrast_obj %>%
    as_tibble() %>%
    mutate(source = contrast %>% str_replace_all(" - Human", "") %>% str_replace_all("\\(|\\)", "") %>% str_trim()) %>%
    select(source, trial_nudge, contrast_p_value = p.value)

  emm_data %>%
    left_join(
      contrast_data,
      by = c("source", "trial_nudge")
    )
}

add_significance_stars <- function(df, p_col = "p.value") {
  df %>%
    mutate(
      sig_stars = case_when(
        .data[[p_col]] < 0.0001 ~ "****",
        .data[[p_col]] < 0.001 ~ "***",
        .data[[p_col]] < 0.01 ~ "**",
        .data[[p_col]] < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}
