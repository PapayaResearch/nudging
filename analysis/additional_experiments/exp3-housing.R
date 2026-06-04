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

source("utils.R")
source("marginals.R")
library(patchwork)

ensure_dirs(c("figures", "results", "tables"))

# ============================================================================
# SETUP
# ============================================================================

HOUSING_SOURCE_LEVELS <- c(
  "Claude 3 Haiku",
  "Claude 4.5 Sonnet",
  "GPT-4o",
  "GPT-5",
  "GPT-5 Mini",
  "o3 Mini"
)

to_binary <- function(x) {
  case_when(
    x %in% c(TRUE, "True", 1L, 1) ~ 1,
    x %in% c(FALSE, "False", 0L, 0) ~ 0
  )
}

add_significance_stars <- function(
  data,
  p_col = "p_value"
) {
  data %>%
    mutate(
      sig_stars = case_when(
        is.na(.data[[p_col]]) ~ "",
        .data[[p_col]] < 0.0001 ~ "****",
        .data[[p_col]] < 0.001 ~ "***",
        .data[[p_col]] < 0.01 ~ "**",
        .data[[p_col]] < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

summarize_binary_rate <- function(
  data,
  outcome_var,
  group_vars
) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      successes = sum(.data[[outcome_var]], na.rm = TRUE),
      n = sum(!is.na(.data[[outcome_var]])),
      prob = successes / n,
      se = sqrt(prob * (1 - prob) / n),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      lower_ci = binom.test(successes, n)$conf.int[1],
      upper_ci = binom.test(successes, n)$conf.int[2]
    ) %>%
    ungroup()
}

fit_binary_sensitivity_model <- function(
  data,
  outcome_var = "chose_nudge",
  condition_var = "trial_nudge"
) {
  feglm(
    as.formula(sprintf("%s ~ source * %s", outcome_var, condition_var)),
    data = data,
    family = binomial(),
    vcov = ~ trial_num
  )
}

compute_model_sensitivity <- function(
  data,
  experiment,
  condition_var = "trial_nudge",
  comparison_method = "revpairwise"
) {
  emm <- get_marginal_effects(
    fit_binary_sensitivity_model(
      data = data,
      condition_var = condition_var
    ),
    as.formula(sprintf("~ source | %s", condition_var)),
    data = data
  )

  emm %>%
    regrid() %>%
    contrast(method = comparison_method, by = "source") %>%
    summary(infer = TRUE) %>%
    as_tibble() %>%
    rename(
      comparison = contrast,
      lower_ci = asymp.LCL,
      upper_ci = asymp.UCL,
      p_value = p.value
    ) %>%
    separate(
      comparison,
      into = c("focal_condition", "reference_condition"),
      sep = " - "
    ) %>%
    mutate(
      comparison = paste(focal_condition, reference_condition, sep = " - "),
      experiment = experiment,
      vcov = "trial_num"
    ) %>%
    select(
      source,
      experiment,
      reference_condition,
      focal_condition,
      comparison,
      estimate,
      lower_ci,
      upper_ci,
      p_value,
      SE,
      z.ratio,
      vcov
    )
}

get_highlight_utility_emm <- function(
  data
) {
  model <- feglm(
    chose_nudge ~ source * trial_nudge * is_nudge_index_optimal,
    data = data,
    family = binomial(),
    vcov = ~ trial_num
  )

  get_marginal_effects(
    model,
    ~ trial_nudge * is_nudge_index_optimal | source,
    data = data
  )
}

compute_highlight_utility_sensitivity <- function(
  data
) {
  get_highlight_utility_emm(data = data) %>%
    regrid() %>%
    contrast(method = "revpairwise", by = c("source", "is_nudge_index_optimal")) %>%
    summary(infer = TRUE) %>%
    as_tibble() %>%
    rename(
      comparison = contrast,
      lower_ci = asymp.LCL,
      upper_ci = asymp.UCL,
      p_value = p.value
    ) %>%
    separate(
      comparison,
      into = c("focal_condition", "reference_condition"),
      sep = " - "
    ) %>%
    mutate(
      comparison = paste(focal_condition, reference_condition, sep = " - "),
      experiment = "Highlight",
      vcov = "trial_num"
    ) %>%
    select(
      source,
      experiment,
      is_nudge_index_optimal,
      reference_condition,
      focal_condition,
      comparison,
      estimate,
      lower_ci,
      upper_ci,
      p_value,
      SE,
      z.ratio,
      vcov
    )
}

make_rate_plot <- function(
  data,
  color_var,
  ylab,
  legend_title,
  palette_values,
  facet_var = NULL
) {
  dodge <- position_dodge(width = 0.5)

  plot_obj <- data %>%
    ggplot(aes(
      x = source,
      y = prob,
      color = .data[[color_var]],
      group = .data[[color_var]]
    )) +
    geom_errorbar(
      aes(
        ymin = lower_ci,
        ymax = upper_ci
      ),
      position = dodge,
      width = 0.15,
      linewidth = 0.4
    ) +
    geom_point(
      position = dodge,
      size = 1.8
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      breaks = seq(0.25, 1, by = 0.25),
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    scale_color_manual(values = palette_values) +
    coord_flip() +
    xlab("Model") +
    ylab(ylab) +
    guides(color = guide_legend(title = legend_title)) +
    theme_nudge()

  if (!is.null(facet_var)) {
    plot_obj <- plot_obj + facet_wrap(as.formula(paste("~", facet_var)))
  }

  plot_obj
}

write_housing_table <- function(
  data,
  caption,
  output.path,
  label,
  escape = TRUE
) {
  data %>%
    kbl("markdown") %>%
    write_lines(output.path %>% str_replace("\\.tex$", ".md"))

  data %>%
    kbl(
      "latex",
      booktabs = TRUE,
      linesep = "",
      caption = caption,
      escape = escape,
      position = "!htb",
      label = label
    ) %>%
    write_lines(output.path)
}

make_rate_table <- function(
  data,
  names_from,
  values_select,
  caption,
  output.path,
  id_cols = "source",
  rename_cols = c(Model = "source"),
  label = "tab:housing-rate-table"
) {
  table_data <- data %>%
    mutate(
      rate_summary = paste0(
        round(prob * 100, 1), "% [",
        round(lower_ci * 100, 1), ", ",
        round(upper_ci * 100, 1), "] (n=",
        n,
        ")"
      )
    ) %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = {{ names_from }},
      values_from = rate_summary
    ) %>%
    select(all_of(id_cols), all_of(values_select)) %>%
    rename(!!!rename_cols)

  write_housing_table(
    data = table_data,
    caption = caption,
    output.path = output.path,
    label = label
  )
}

make_effect_table <- function(
  data,
  names_from,
  values_select,
  caption,
  output.path,
  id_cols = "source",
  rename_cols = c(Model = "source"),
  label = "tab:housing-effect-table",
  p_col = "p_value"
) {
  table_data <- data %>%
    add_significance_stars(p_col = p_col) %>%
    mutate(
      effect_summary = case_when(
        sig_stars == "" ~ paste0(
          round(estimate * 100, 1), " pp [",
          round(lower_ci * 100, 1), ", ",
          round(upper_ci * 100, 1), "]"
        ),
        TRUE ~ paste0(
          round(estimate * 100, 1), "$^{", sig_stars, "}$ pp [",
          round(lower_ci * 100, 1), ", ",
          round(upper_ci * 100, 1), "]"
        )
      )
    ) %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = {{ names_from }},
      values_from = effect_summary
    ) %>%
    select(all_of(id_cols), all_of(values_select)) %>%
    rename(!!!rename_cols)

  write_housing_table(
    data = table_data,
    caption = caption,
    output.path = output.path,
    label = label,
    escape = FALSE
  )
}

make_single_rate_table <- function(
  data,
  caption,
  output.path,
  value_name,
  label = "tab:housing-single-rate-table"
) {
  table_data <- data %>%
    mutate(
      rate_summary = paste0(
        round(prob * 100, 1), "% [",
        round(lower_ci * 100, 1), ", ",
        round(upper_ci * 100, 1), "] (n=",
        n,
        ")"
      )
    ) %>%
    transmute(
      Model = source,
      !!value_name := rate_summary
    )

  write_housing_table(
    data = table_data,
    caption = caption,
    output.path = output.path,
    label = label
  )
}

make_global_marginal_table <- function(
  default_data,
  highlight_data,
  suggestion_data,
  highlight_effect_data,
  caption,
  output.path,
  label = "tab:housing-nudge-marginals"
) {
  format_marginal_summary <- function(
    data,
    condition_col,
    sig_condition = NULL
  ) {
    formatted <- data %>%
      mutate(
        summary = paste0(
          round(prob * 100, 1), "% [",
          round(asymp.LCL * 100, 1), ", ",
          round(asymp.UCL * 100, 1), "]"
        )
      )

    if (!is.null(sig_condition)) {
      formatted <- formatted %>%
        mutate(
          summary = ifelse(
            .data[[condition_col]] == sig_condition & sig_stars != "",
            paste0(
              round(prob * 100, 1), "$^{", sig_stars, "}$% [",
              round(asymp.LCL * 100, 1), ", ",
              round(asymp.UCL * 100, 1), "]"
            ),
            summary
          )
        )
    }

    formatted %>%
      select(source, condition = all_of(condition_col), summary)
  }

  format_effect_summary <- function(
    data,
    condition_col
  ) {
    data %>%
      mutate(
        summary = ifelse(
          sig_stars == "",
          paste0(
            round(estimate * 100, 1), " pp [",
            round(lower_ci * 100, 1), ", ",
            round(upper_ci * 100, 1), "]"
          ),
          paste0(
            round(estimate * 100, 1), "$^{", sig_stars, "}$ pp [",
            round(lower_ci * 100, 1), ", ",
            round(upper_ci * 100, 1), "]"
          )
        )
      ) %>%
      select(source, condition = all_of(condition_col), summary)
  }

  default_table <- format_marginal_summary(
    data = default_data,
    condition_col = "trial_nudge",
    sig_condition = "Pres."
  ) %>%
    pivot_wider(
      id_cols = "source",
      names_from = condition,
      names_prefix = "Default: ",
      values_from = summary
    )

  highlight_table <- format_marginal_summary(
    data = highlight_data,
    condition_col = "is_nudge_index_optimal",
    sig_condition = "Suboptimal"
  ) %>%
    pivot_wider(
      id_cols = "source",
      names_from = condition,
      names_prefix = "Highlight: ",
      values_from = summary
    )

  suggestion_table <- format_marginal_summary(
    data = suggestion_data,
    condition_col = "trial_nudge",
    sig_condition = "Late"
  ) %>%
    pivot_wider(
      id_cols = "source",
      names_from = condition,
      names_prefix = "Suggestion: ",
      values_from = summary
    )

  highlight_effect_table <- format_effect_summary(
    data = highlight_effect_data,
    condition_col = "is_nudge_index_optimal"
  ) %>%
    pivot_wider(
      id_cols = "source",
      names_from = condition,
      names_prefix = "Highlight Effect: ",
      values_from = summary
    )

  table_data <- default_table %>%
    left_join(highlight_table, by = "source") %>%
    left_join(suggestion_table, by = "source") %>%
    left_join(highlight_effect_table, by = "source") %>%
    mutate(source = factor(as.character(source), levels = HOUSING_SOURCE_LEVELS)) %>%
    arrange(source) %>%
    select(
      source,
      `Default: Abs.`,
      `Default: Pres.`,
      `Highlight: Optimal`,
      `Highlight: Suboptimal`,
      `Suggestion: Early`,
      `Suggestion: Late`,
      `Highlight Effect: Optimal`,
      `Highlight Effect: Suboptimal`
    ) %>%
    rename(Model = source)

  write_housing_table(
    data = table_data,
    caption = caption,
    output.path = output.path,
    label = label,
    escape = FALSE
  )
}

required_files <- c(
  "data/data-housing-default.csv",
  "data/data-housing-highlight.csv",
  "data/data-housing-suggestion.csv"
)

if (!all(file.exists(required_files))) {
  stop("Missing preprocessed housing data. Run `python3 preprocess_housing.py` from `nudging/analysis` first.")
}

# ============================================================================
# LOAD DATA
# ============================================================================

data_default <- read.csv("data/data-housing-default.csv") %>%
  preprocess_data(nudge_type = "default") %>%
  mutate(
    source = factor(as.character(source), levels = HOUSING_SOURCE_LEVELS),
    chose_nudge = to_binary(chose_nudge),
    is_nudge_index_optimal = ifelse(nudge_index == optimal_option, "Optimal", "Suboptimal") %>%
      factor(levels = c("Optimal", "Suboptimal"))
  ) %>%
  droplevels()

data_highlight <- read.csv("data/data-housing-highlight.csv") %>%
  preprocess_data(nudge_type = "highlight") %>%
  mutate(
    source = factor(as.character(source), levels = HOUSING_SOURCE_LEVELS),
    chose_nudge = is_first_index_nudged,
    trial_nudge = factor(trial_nudge, levels = c("Abs.", "Pres."))
  ) %>%
  droplevels()

data_suggestion <- read.csv("data/data-housing-suggestion.csv") %>%
  preprocess_data(nudge_type = "suggestion") %>%
  mutate(
    source = factor(as.character(source), levels = HOUSING_SOURCE_LEVELS),
    chose_nudge = to_binary(chose_nudge),
    is_nudge_index_optimal = case_when(
      trial_nudge == "Abs." ~ NA_character_,
      nudge_index == optimal_option ~ "Optimal",
      TRUE ~ "Suboptimal"
    ) %>% factor(levels = c("Optimal", "Suboptimal"))
  ) %>%
  droplevels()

# ============================================================================
# RATE SUMMARIES
# ============================================================================

rate_default <- summarize_binary_rate(
  data = data_default,
  outcome_var = "chose_nudge",
  group_vars = c("source", "trial_nudge")
) %>%
  mutate(experiment = "Default")

rate_highlight <- summarize_binary_rate(
  data = data_highlight,
  outcome_var = "chose_nudge",
  group_vars = c("source", "trial_nudge")
) %>%
  mutate(experiment = "Highlight")

rate_highlight_main <- summarize_binary_rate(
  data = data_highlight %>%
    filter(trial_nudge == "Pres.") %>%
    droplevels(),
  outcome_var = "chose_nudge",
  group_vars = c("source", "is_nudge_index_optimal")
) %>%
  mutate(experiment = "Highlight")

rate_suggestion <- summarize_binary_rate(
  data = data_suggestion %>%
    filter(trial_nudge != "Abs.") %>%
    droplevels(),
  outcome_var = "chose_nudge",
  group_vars = c("source", "trial_nudge")
) %>%
  mutate(experiment = "Suggestion")

rate_all <- bind_rows(
  rate_default,
  rate_highlight,
  rate_suggestion
)

# ============================================================================
# NUDGE SENSITIVITY
# ============================================================================

sensitivity_default <- compute_model_sensitivity(
  data = data_default,
  experiment = "Default"
)

sensitivity_highlight <- compute_model_sensitivity(
  data = data_highlight %>%
    filter(trial_nudge == "Pres.") %>%
    droplevels(),
  experiment = "Highlight",
  condition_var = "is_nudge_index_optimal"
)

sensitivity_suggestion <- compute_model_sensitivity(
  data = data_suggestion %>%
    filter(trial_nudge != "Abs.") %>%
    droplevels(),
  experiment = "Suggestion"
)

sensitivity_all <- bind_rows(
  sensitivity_default,
  sensitivity_highlight,
  sensitivity_suggestion
) %>%
  add_significance_stars()

# ============================================================================
# SECONDARY: UTILITY-BASED ANALYSIS
# ============================================================================

utility_default <- summarize_binary_rate(
  data = data_default %>% filter(trial_nudge == "Pres.") %>% droplevels(),
  outcome_var = "chose_nudge",
  group_vars = c("source", "is_nudge_index_optimal")
) %>%
  mutate(experiment = "Default")

utility_highlight <- summarize_binary_rate(
  data = data_highlight,
  outcome_var = "chose_nudge",
  group_vars = c("source", "trial_nudge", "is_nudge_index_optimal")
) %>%
  mutate(experiment = "Highlight")

utility_highlight_sensitivity <- compute_highlight_utility_sensitivity(
  data = data_highlight
) %>%
  add_significance_stars()

utility_suggestion <- summarize_binary_rate(
  data = data_suggestion %>% filter(trial_nudge != "Abs.") %>% droplevels(),
  outcome_var = "chose_nudge",
  group_vars = c("source", "trial_nudge", "is_nudge_index_optimal")
) %>%
  mutate(experiment = "Suggestion")

utility_bad_switch <- summarize_binary_rate(
  data = data_suggestion %>%
    filter(
      trial_nudge == "Late",
      selected_option != first_selected_option
    ) %>%
    mutate(
      switched_worse = as.integer(value_final_option_selected < value_first_option_selected)
    ),
  outcome_var = "switched_worse",
  group_vars = "source"
) %>%
  mutate(experiment = "Suggestion")

emm_default <- get_marginal_effects(
  fit_binary_sensitivity_model(data = data_default),
  ~ source | trial_nudge,
  data = data_default
) %>%
  as_tibble() %>%
  left_join(
    sensitivity_default %>%
      add_significance_stars() %>%
      select(source, sig_stars),
    by = "source"
  )

emm_highlight <- get_marginal_effects(
  fit_binary_sensitivity_model(
    data = data_highlight %>%
      filter(trial_nudge == "Pres.") %>%
      droplevels(),
    condition_var = "is_nudge_index_optimal"
  ),
  ~ source | is_nudge_index_optimal,
  data = data_highlight %>%
    filter(trial_nudge == "Pres.") %>%
    droplevels()
) %>%
  as_tibble() %>%
  left_join(
    sensitivity_highlight %>%
      add_significance_stars() %>%
      select(source, sig_stars),
    by = "source"
  )

emm_suggestion <- get_marginal_effects(
  fit_binary_sensitivity_model(
    data = data_suggestion %>%
      filter(trial_nudge != "Abs.") %>%
      droplevels()
  ),
  ~ source | trial_nudge,
  data = data_suggestion %>%
    filter(trial_nudge != "Abs.") %>%
    droplevels()
) %>%
  as_tibble() %>%
  left_join(
    sensitivity_suggestion %>%
      add_significance_stars() %>%
      select(source, sig_stars),
    by = "source"
  )

# ============================================================================
# SAVE RESULTS
# ============================================================================

write_csv(rate_default, "results/housing-default-rates.csv")
write_csv(rate_highlight, "results/housing-highlight-rates.csv")
write_csv(rate_highlight_main, "results/housing-highlight-main-rates.csv")
write_csv(rate_suggestion, "results/housing-suggestion-rates.csv")
write_csv(rate_all, "results/housing-all-rates.csv")

write_csv(sensitivity_default, "results/housing-default-sensitivity.csv")
write_csv(sensitivity_highlight, "results/housing-highlight-sensitivity.csv")
write_csv(sensitivity_suggestion, "results/housing-suggestion-sensitivity.csv")
write_csv(sensitivity_all, "results/housing-all-sensitivity.csv")

write_csv(utility_default, "results/housing-default-utility-rates.csv")
write_csv(utility_highlight, "results/housing-highlight-utility-rates.csv")
write_csv(utility_highlight_sensitivity, "results/housing-highlight-utility-sensitivity.csv")
write_csv(utility_suggestion, "results/housing-suggestion-utility-rates.csv")
write_csv(utility_bad_switch, "results/housing-suggestion-bad-switch-utility.csv")

# ============================================================================
# TABLES
# ============================================================================

make_rate_table(
  data = rate_default,
  names_from = trial_nudge,
  values_select = c("Abs.", "Pres."),
  caption = "Observed housing default-choice rates with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-default-rates.tex",
  label = "housing-default-rates"
)

make_rate_table(
  data = rate_highlight_main,
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Observed housing highlight-target rates by target-row optimality among present highlights, with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-highlight-rates.tex",
  label = "housing-highlight-rates"
)

make_rate_table(
  data = rate_suggestion,
  names_from = trial_nudge,
  values_select = c("Early", "Late"),
  caption = "Observed housing suggestion-choice rates with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-suggestion-rates.tex",
  label = "housing-suggestion-rates"
)

make_global_marginal_table(
  default_data = emm_default,
  highlight_data = emm_highlight,
  suggestion_data = emm_suggestion,
  highlight_effect_data = utility_highlight_sensitivity,
  caption = "Model-based housing marginal summaries corresponding to the four panels in the housing nudge marginals figure. Default, highlight, and suggestion entries report estimated marginal means with 95\\% confidence intervals; highlight effect entries report present-minus-absent effects in percentage points. Stars denote the same displayed model-specific contrasts as in the figure.",
  output.path = "tables/housing-nudge-marginals.tex",
  label = "housing-nudge-marginals"
)

make_effect_table(
  data = sensitivity_all,
  names_from = experiment,
  values_select = c("Default", "Highlight", "Suggestion"),
  caption = "Model-specific housing nudge sensitivity estimates in percentage points. Stars denote significance for the displayed contrast.",
  output.path = "tables/housing-nudge-sensitivity.tex",
  label = "housing-nudge-sensitivity"
)

make_rate_table(
  data = utility_default,
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Observed default-choice rates by whether the default row is optimal, with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-default-utility-rates.tex",
  label = "housing-default-utility-rates"
)

make_rate_table(
  data = utility_highlight,
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Observed highlight-target rates by highlight presence and row optimality, with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-highlight-utility-rates.tex",
  id_cols = c("source", "trial_nudge"),
  rename_cols = c(Model = "source", Condition = "trial_nudge"),
  label = "housing-highlight-utility-rates"
)

make_effect_table(
  data = utility_highlight_sensitivity,
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Highlight present-minus-absent effects by target-row optimality in percentage points. Stars denote significance for the displayed contrast.",
  output.path = "tables/housing-highlight-utility-sensitivity.tex",
  label = "housing-highlight-utility-sensitivity"
)

make_rate_table(
  data = utility_suggestion,
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Observed suggestion-choice rates by suggestion timing and row optimality, with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-suggestion-utility-rates.tex",
  id_cols = c("source", "trial_nudge"),
  rename_cols = c(Model = "source", Timing = "trial_nudge"),
  label = "housing-suggestion-utility-rates"
)

make_single_rate_table(
  data = utility_bad_switch,
  caption = "Observed rates of switching to a worse option after a late suggestion-induced switch, with 95\\% binomial confidence intervals.",
  output.path = "tables/housing-utility-bad-switch.tex",
  value_name = "P(Switched Worse | Late Switch)",
  label = "housing-utility-bad-switch"
)

# ============================================================================
# FIGURES
# ============================================================================

p.default <- emm_default %>%
  ggplot(aes(
    source,
    prob,
    color = trial_nudge
  )) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_text(
    data = emm_default %>%
      filter(trial_nudge == "Pres."),
    aes(
      y = asymp.UCL,
      label = sig_stars
    ),
    hjust = -0.2,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_x_discrete(
    limits = HOUSING_SOURCE_LEVELS,
    drop = FALSE
  ) +
  scale_color_aaas() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Follow Nudge)") +
  guides(color = guide_legend(title = "Default Absent/Present")) +
  theme_nudge()

p.highlight <- emm_highlight %>%
  ggplot(aes(
    source,
    prob,
    color = is_nudge_index_optimal
  )) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_text(
    data = emm_highlight %>%
      filter(is_nudge_index_optimal == "Suboptimal"),
    aes(
      y = asymp.UCL,
      label = sig_stars
    ),
    hjust = -0.2,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_x_discrete(
    limits = HOUSING_SOURCE_LEVELS,
    drop = FALSE
  ) +
  scale_color_cosmic() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Follow Nudge)") +
  guides(color = guide_legend(title = "Highlight Optimality")) +
  theme_nudge()

p.suggestion <- emm_suggestion %>%
  ggplot(aes(
    source,
    prob,
    color = trial_nudge
  )) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_text(
    data = emm_suggestion %>%
      filter(trial_nudge == "Late"),
    aes(
      y = asymp.UCL,
      label = sig_stars
    ),
    hjust = -0.2,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_x_discrete(
    limits = HOUSING_SOURCE_LEVELS,
    drop = FALSE
  ) +
  scale_colour_primer() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Follow Nudge)") +
  guides(color = guide_legend(title = "Suggestion Timing")) +
  theme_nudge()

p.highlight.did <- utility_highlight_sensitivity %>%
  ggplot(aes(
    x = source,
    y = estimate,
    color = is_nudge_index_optimal,
    group = is_nudge_index_optimal
  )) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "gray40"
  ) +
  geom_pointrange(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    size = 0.3
  ) +
  geom_text(
    aes(
      y = ifelse(estimate >= 0, upper_ci, lower_ci),
      label = sig_stars,
      hjust = ifelse(estimate >= 0, -0.2, 1.2)
    ),
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(-1, 1, by = 0.25),
    expand = expansion(mult = c(0.14, 0.14))
  ) +
  scale_x_discrete(
    limits = HOUSING_SOURCE_LEVELS,
    drop = FALSE
  ) +
  scale_color_cosmic() +
  coord_flip() +
  xlab("Model") +
  ylab("Highlight Effect") +
  guides(color = guide_legend(title = "Highlight Optimality")) +
  theme_nudge()

p.marginals <- (p.default | p.highlight | p.suggestion | p.highlight.did) +
  plot_layout(
    guides = "collect",
    axes = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )

p.sensitivity <- sensitivity_all %>%
  ggplot(aes(
    x = source,
    y = estimate,
    color = comparison,
    group = comparison
  )) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "gray40"
  ) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    position = position_dodge(width = 0.5),
    width = 0.15,
    linewidth = 0.4
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 1.8
  ) +
  geom_text(
    aes(
      y = ifelse(estimate >= 0, upper_ci + 0.03, lower_ci - 0.03),
      label = sig_stars,
      hjust = ifelse(estimate >= 0, 0, 1)
    ),
    position = position_dodge(width = 0.5),
    size = 3.5,
    show.legend = FALSE
  ) +
  facet_wrap(~ experiment) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(-1, 1, by = 0.5),
    expand = expansion(mult = c(0.14, 0.14))
  ) +
  scale_color_manual(
    values = c(
      "Pres. - Abs." = "#E15759",
      "Late - Early" = "#59A14F",
      "Suboptimal - Optimal" = "#B07AA1"
    )
  ) +
  coord_flip() +
  xlab("Model") +
  ylab("Nudge Sensitivity") +
  guides(color = guide_legend(title = "Contrast")) +
  theme_nudge()

p.default.utility <- make_rate_plot(
  data = utility_default,
  color_var = "is_nudge_index_optimal",
  ylab = "P(Choose Default)",
  legend_title = "Default Optimality",
  palette_values = c("Optimal" = "#4E79A7", "Suboptimal" = "#E15759")
)

p.highlight.utility <- make_rate_plot(
  data = utility_highlight,
  color_var = "is_nudge_index_optimal",
  ylab = "P(First Reveal Is Target Row)",
  legend_title = "Highlight Optimality",
  palette_values = c("Optimal" = "#4E79A7", "Suboptimal" = "#E15759"),
  facet_var = "trial_nudge"
)

p.suggestion.utility <- make_rate_plot(
  data = utility_suggestion,
  color_var = "is_nudge_index_optimal",
  ylab = "P(Choose Suggestion)",
  legend_title = "Suggestion Optimality",
  palette_values = c("Optimal" = "#4E79A7", "Suboptimal" = "#E15759"),
  facet_var = "trial_nudge"
)

p.utility <- (p.default.utility | p.highlight.utility | p.suggestion.utility) +
  plot_layout(
    guides = "collect",
    axes = "collect",
    widths = c(1, 2, 2)
  ) &
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )

dodge.utility <- position_dodge(width = 0.5)

p.highlight.utility.sensitivity <- utility_highlight_sensitivity %>%
  ggplot(aes(
    x = source,
    y = estimate,
    color = is_nudge_index_optimal,
    group = is_nudge_index_optimal
  )) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "gray40"
  ) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    position = dodge.utility,
    width = 0.15,
    linewidth = 0.4
  ) +
  geom_point(
    position = dodge.utility,
    size = 1.8
  ) +
  geom_text(
    aes(
      y = ifelse(estimate >= 0, upper_ci + 0.03, lower_ci - 0.03),
      label = sig_stars,
      hjust = ifelse(estimate >= 0, 0, 1)
    ),
    position = dodge.utility,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(-1, 1, by = 0.25),
    expand = expansion(mult = c(0.14, 0.14))
  ) +
  scale_color_manual(values = c("Optimal" = "#4E79A7", "Suboptimal" = "#E15759")) +
  coord_flip() +
  xlab("Model") +
  ylab("Highlight Effect (Pres. - Abs.)") +
  guides(color = guide_legend(title = "Target Row Optimality")) +
  theme_nudge()

p.utility.bad_switch <- utility_bad_switch %>%
  mutate(
    source_label = factor(
      paste0(source, " (n=", n, ")"),
      levels = paste0(as.character(source), " (n=", n, ")")
    )
  ) %>%
  ggplot(aes(
    x = source_label,
    y = prob
  )) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    width = 0.15,
    linewidth = 0.4
  ) +
  geom_point(size = 1.8) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_flip() +
  xlab("Model") +
  ylab("P(Switched to Worse Option if Switched Late)") +
  theme_nudge()

p.marginals %>%
  ggsave(
    filename = "figures/housing-nudge-marginals.pdf",
    plot = .,
    width = 10.5,
    height = 4
  )

p.sensitivity %>%
  ggsave(
    filename = "figures/housing-nudge-sensitivity.pdf",
    plot = .,
    width = 8,
    height = 4
  )

p.utility %>%
  ggsave(
    filename = "figures/housing-utility-optimality.pdf",
    plot = .,
    width = 10,
    height = 4
  )

p.utility.bad_switch %>%
  ggsave(
    filename = "figures/housing-utility-bad-switch.pdf",
    plot = .,
    width = 6,
    height = 2.4
  )

p.highlight.utility.sensitivity %>%
  ggsave(
    filename = "figures/housing-highlight-utility-sensitivity-optimality.pdf",
    plot = .,
    width = 5,
    height = 4
  )
 
