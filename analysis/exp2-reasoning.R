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

ensure_dirs(c("figures", "tables", "results"))

# ============================================================================
# LOAD DATA
# ============================================================================

data_default <- read.csv("data/data-default.csv") %>%
  preprocess_data(nudge_type = "default", reasoning_evals = TRUE) %>%
  mutate(chose_nudge = case_when(chose_nudge == "True" ~ 1, chose_nudge == "False" ~ 0)) %>%
  droplevels() %>%
  sep_model_and_reasoning() %>%
  mutate(
    model = model %>% relevel(ref = "Human"),
    reasoning_effort = reasoning_effort %>% relevel(ref = "Unknown"),
    total_tokens = total_tokens %>% python_list_to_mean(),
    prompt_tokens = prompt_tokens %>% python_list_to_mean(),
    completion_tokens = completion_tokens %>% python_list_to_mean(),
    reasoning_tokens = reasoning_tokens %>% python_list_to_mean(),
    cost = get_cost_per_token_for_model(model, "output") * (completion_tokens + reasoning_tokens)
  )

data_highlight <- read.csv("data/data-highlight.csv") %>%
  preprocess_data(nudge_type = "highlight", reasoning_evals = TRUE) %>%
  rename(chose_nudge = is_first_index_nudged) %>%
  droplevels() %>%
  sep_model_and_reasoning() %>%
  mutate(
    model = model %>% relevel(ref = "Human"),
    reasoning_effort = reasoning_effort %>% relevel(ref = "Unknown"),
    total_tokens = total_tokens %>% python_list_to_mean(),
    prompt_tokens = prompt_tokens %>% python_list_to_mean(),
    completion_tokens = completion_tokens %>% python_list_to_mean(),
    reasoning_tokens = reasoning_tokens %>% python_list_to_mean(),
    cost = get_cost_per_token_for_model(model, "output") * (completion_tokens + reasoning_tokens)
  )

data_suggestion <- read.csv("data/data-suggestion.csv") %>%
  preprocess_data(nudge_type = "suggestion", reasoning_evals = TRUE) %>%
  mutate(chose_nudge = case_when(chose_nudge == "True" ~ 1, chose_nudge == "False" ~ 0)) %>%
  droplevels() %>%
  sep_model_and_reasoning() %>%
  mutate(
    model = model %>% relevel(ref = "Human"),
    reasoning_effort = reasoning_effort %>% relevel(ref = "Unknown"),
    total_tokens = total_tokens %>% python_list_to_mean(),
    prompt_tokens = prompt_tokens %>% python_list_to_mean(),
    completion_tokens = completion_tokens %>% python_list_to_mean(),
    reasoning_tokens = reasoning_tokens %>% python_list_to_mean(),
    cost = get_cost_per_token_for_model(model, "output") * (completion_tokens + reasoning_tokens)
  )

# ============================================================================
# REASONING TOKEN ANALYSIS
# ============================================================================

model_reasoning_default <- fit_reasoning_model(data_default)
model_reasoning_highlight <- fit_reasoning_model(data_highlight)
model_reasoning_suggestion <- fit_reasoning_model(data_suggestion)

model_reasoning_default.total <- fit_reasoning_model(data_default, outcome.var = "total_tokens")
model_reasoning_highlight.total <- fit_reasoning_model(data_highlight, outcome.var = "total_tokens")
model_reasoning_suggestion.total <- fit_reasoning_model(data_suggestion, outcome.var = "total_tokens")

emm_reasoning_default <- model_reasoning_default %>%
  compute_emmeans_pairwise(data_default %>% subset(source != "Human") %>% droplevels()) %>%
  mutate(nudge_type = "Default")

emm_reasoning_highlight <- model_reasoning_highlight %>%
  compute_emmeans_pairwise(data_highlight %>% subset(source != "Human") %>% droplevels()) %>%
  mutate(nudge_type = "Highlight")

emm_reasoning_suggestion <- model_reasoning_suggestion %>%
  compute_emmeans_trt_vs_ctrl(data_suggestion %>% subset(source != "Human") %>% droplevels()) %>%
  mutate(nudge_type = "Suggestion")

emm_reasoning <- bind_rows(
  emm_reasoning_default,
  emm_reasoning_highlight,
  emm_reasoning_suggestion
) %>%
  mutate(nudge_type = factor(nudge_type, levels = c("Default", "Highlight", "Suggestion")))

if ("prob" %in% names(emm_reasoning)) {
  emm_reasoning <- emm_reasoning %>% rename(emmean = prob)
}
if ("asymp.LCL" %in% names(emm_reasoning)) {
  emm_reasoning <- emm_reasoning %>% rename(lower.CL = asymp.LCL)
}
if ("asymp.UCL" %in% names(emm_reasoning)) {
  emm_reasoning <- emm_reasoning %>% rename(upper.CL = asymp.UCL)
}

# ============================================================================
# DEFAULT EXPERIMENT
# ============================================================================

data_default_prep <- data_default %>%
  mutate(chose_nudge = recode(chose_nudge, "True" = 1, "False" = 0))

model_default <- feglm(
  chose_nudge ~ model * reasoning_effort * trial_nudge,
  data = data_default_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm_default <- get_marginal_effects(
  model_default,
  ~ model + reasoning_effort | trial_nudge,
  data = data_default_prep
)

contrasts_default <- emm_default %>%
  compute_human_contrasts(by_vars = "trial_nudge") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human Unknown", ""),
    reasoning_effort = contrast %>% str_extract("Minimal|Low|Medium"),
    model = contrast %>% str_replace_all("Minimal|Low|Medium", "") %>% str_trim()
  ) %>% select(model, reasoning_effort, trial_nudge, p.value)

emm_default_diff <- emm_default %>%
  regrid() %>%
  contrast(method = "revpairwise", by = c("model", "reasoning_effort")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(trial_nudge = contrast)

emm_default <- emm_default %>% as_tibble() %>%
  left_join(
    contrasts_default,
    by = c("model", "reasoning_effort", "trial_nudge")
  ) %>%
  rename(contrast_p_value = p.value)

# ============================================================================
# SUGGESTION EXPERIMENT
# ============================================================================

data_suggestion_prep <- data_suggestion %>%
  filter(trial_nudge != "Abs.") %>%
  droplevels() %>%
  mutate(chose_nudge = recode(chose_nudge, "True" = 1, "False" = 0))

model_suggestion <- feglm(
  chose_nudge ~ model * trial_nudge * reasoning_effort,
  data = data_suggestion_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm_suggestion <- get_marginal_effects(
  model_suggestion,
  ~ model + reasoning_effort | trial_nudge,
  data = data_suggestion_prep
)

contrasts_suggestion <- emm_suggestion %>%
  compute_human_contrasts(by_vars = "trial_nudge") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human Unknown", ""),
    reasoning_effort = contrast %>% str_extract("Minimal|Low|Medium"),
    model = contrast %>% str_replace_all("Minimal|Low|Medium", "") %>% str_trim()
  ) %>% select(model, reasoning_effort, trial_nudge, p.value)

emm_suggestion_diff <- emm_suggestion %>%
  regrid() %>%
  contrast(method = "pairwise", by = c("model", "reasoning_effort")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(trial_nudge = contrast)

emm_suggestion <- emm_suggestion %>% as_tibble() %>%
  left_join(
    contrasts_suggestion,
    by = c("model", "reasoning_effort", "trial_nudge")
  ) %>%
  rename(contrast_p_value = p.value)

# ============================================================================
# HIGHLIGHT EXPERIMENT
# ============================================================================

data_highlight_prep <- data_highlight %>%
  filter(trial_nudge == "Pres.")

model_highlight <- feglm(
  chose_nudge ~ model * reasoning_effort * is_nudge_index_optimal,
  data = data_highlight_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm_highlight <- get_marginal_effects(
  model_highlight,
  ~ model + reasoning_effort | is_nudge_index_optimal,
  data = data_highlight_prep
)

emm_highlight_diff <- emm_highlight %>%
  regrid() %>%
  contrast(method = "revpairwise", by = c("model", "reasoning_effort")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(is_nudge_index_optimal = contrast)

contrasts_highlight <- emm_highlight %>%
  compute_human_contrasts(by_vars = "is_nudge_index_optimal") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human Unknown", ""),
    reasoning_effort = contrast %>% str_extract("Minimal|Low|Medium"),
    model = contrast %>% str_replace_all("Minimal|Low|Medium", "") %>% str_trim()
  ) %>% select(model, reasoning_effort, is_nudge_index_optimal, p.value)

emm_highlight <- emm_highlight %>% as_tibble() %>%
  left_join(
    contrasts_highlight,
    by = c("model", "reasoning_effort", "is_nudge_index_optimal")
  ) %>%
  rename(contrast_p_value = p.value)

diffs_combined <- rbind(
  emm_default_diff %>%
    mutate(nudge_type = "Default") %>%
    rename(contrast = trial_nudge),
  emm_highlight_diff %>%
    mutate(nudge_type = "Highlight") %>%
    rename(contrast = is_nudge_index_optimal),
  emm_suggestion_diff %>%
    mutate(nudge_type = "Suggestion") %>%
    rename(contrast = trial_nudge)
) %>%
  mutate(nudge_type = factor(nudge_type, levels = c("Default", "Highlight", "Suggestion")))

# ============================================================================
# FIGURES
# ============================================================================

p.nudge_default <- emm_default %>%
  mutate(
    nudge_type = "Default",
    reasoning_effort = factor(reasoning_effort, levels = c("Unknown", "Medium", "Low", "Minimal"))
  ) %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(interaction(model, reasoning_effort, sep = " / ", lex.order = TRUE), model == "Human"), prob, color = trial_nudge)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_default %>% subset(model == "Human" & trial_nudge == "Abs.") %>% pull(prob),
    linetype = "dashed",
    color = pal_aaas()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_default %>% subset(model == "Human" & trial_nudge == "Pres.") %>% pull(prob),
    linetype = "dashed",
    color = pal_aaas()(2)[2]
  ) +
  # geom_text(
  #   aes(
  #     y = ifelse(trial_nudge == "Abs.", asymp.LCL, asymp.UCL),
  #     label = sig_stars,
  #     hjust = ifelse(trial_nudge == "Abs.", 1.2, -0.2),
  #   ),
  #   size = 4,
  #   vjust = 0.8,
  #   show.legend = FALSE
  # ) +
  facet_wrap(~ nudge_type) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_color_aaas() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Choose Nudge)") +
  guides(color = guide_legend(title = "Default Absent/Present")) +
  theme_nudge() +
  theme(axis.text.y = element_text(hjust = 0))


p.nudge_suggestion <- emm_suggestion %>%
  mutate(
    nudge_type = "Suggestion",
    reasoning_effort = factor(reasoning_effort, levels = c("Unknown", "Medium", "Low", "Minimal"))
  ) %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(interaction(model, reasoning_effort, sep = " / ", lex.order = TRUE), model == "Human"), prob, color = trial_nudge)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_suggestion %>% subset(model == "Human" & trial_nudge == "Early") %>% pull(prob),
    linetype = "dashed",
    color = pal_primer()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_suggestion %>% subset(model == "Human" & trial_nudge == "Late") %>% pull(prob),
    linetype = "dashed",
    color = pal_primer()(2)[2]
  ) +
  # geom_text(
  #   aes(
  #     y = ifelse(trial_nudge == "Late", asymp.LCL, asymp.UCL),
  #     label = sig_stars,
  #     hjust = ifelse(trial_nudge == "Late", 1.2, -0.2),
  #   ),
  #   size = 4,
  #   vjust = 0.8,
  #   show.legend = FALSE
  # ) +
  facet_wrap(~ nudge_type) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_colour_primer() +
  guides(color = guide_legend(title = "Suggestion Timing")) +
  coord_flip() +
  xlab("Model") +
  ylab("P(Choose Nudge)") +
  theme_nudge() +
  theme(axis.text.y = element_text(hjust = 0))


p.nudge_highlight <- emm_highlight %>%
  mutate(
    nudge_type = "Highlight",
    reasoning_effort = factor(reasoning_effort, levels = c("Unknown", "Medium", "Low", "Minimal"))
  ) %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(interaction(model, reasoning_effort, sep = " / ", lex.order = TRUE), model == "Human"), prob, color = is_nudge_index_optimal)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_highlight %>% subset(model == "Human" & is_nudge_index_optimal == "Optimal") %>% pull(prob),
    linetype = "dashed",
    color = pal_cosmic()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_highlight %>% subset(model == "Human" & is_nudge_index_optimal == "Suboptimal") %>% pull(prob),
    linetype = "dashed",
    color = pal_cosmic()(2)[2]
  ) +
  # geom_text(
  #   aes(
  #     y = ifelse(is_nudge_index_optimal == "Suboptimal", asymp.LCL, asymp.UCL),
  #     label = sig_stars,
  #     hjust = ifelse(is_nudge_index_optimal == "Suboptimal", 1.2, -0.2),
  #   ),
  #   size = 4,
  #   vjust = 0.8,
  #   show.legend = FALSE
  # ) +
  facet_wrap(~ nudge_type) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_color_cosmic() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Choose Nudge)") +
  guides(color = guide_legend(title = "Highlight Optimality")) +
  theme_nudge() +
  theme(axis.text.y = element_text(hjust = 0))


p.nudge_combined <- (p.nudge_default | p.nudge_suggestion | p.nudge_highlight) +
  plot_layout(guides = "collect", nrow = 1, axes = "collect") &
  theme(legend.position = "bottom", legend.direction = "vertical")

p.nudge_combined %>%
  ggsave(
    "figures/reasoning-nudge-choice.pdf",
    plot = .,
    width = 8,
    height = 4
  )


p.reasoning_combined <- emm_reasoning %>%
  mutate(
    reasoning_effort = factor(reasoning_effort, levels = c("Minimal", "Low", "Medium", "Unknown")),
    nudge_type = factor(nudge_type, levels = c("Default", "Suggestion", "Highlight"))
  ) %>%
  ggplot(aes(
    x = trial_nudge,
    y = emmean,
    color = model,
    group = interaction(model, reasoning_effort),
    linetype = reasoning_effort
  )) +
  geom_pointrange(
    aes(ymin = lower.CL, ymax = upper.CL),
    size = 0.2,
    show.legend = FALSE,
    linetype = "solid"
  ) +
  geom_line(
    size = 0.8
  ) +
  guides(
    color = guide_legend(title = "Model"),
    linetype = guide_legend(title = "Reasoning effort")
  ) +
  xlab("Nudge") +
  facet_wrap(~ nudge_type, scales = "free_x") +
  scale_y_continuous(
    name = "Est. avg. reasoning tokens per step",
    trans = "log1p",
    breaks = c(0, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
  ) +
  scale_color_npg() +
  scale_linetype_manual(values = c("Minimal" = "dotted", "Low" = "dashed", "Medium" = "solid", "Unknown" = "solid")) +
  theme_nudge() +
  theme(legend.position = "bottom")

p.reasoning_combined %>%
  ggsave(
    "figures/reasoning-tokens.pdf",
    plot = .,
    width = 8.6,
    height = 4
  )

# ============================================================================
# TABLES
# ============================================================================

model_reasoning_default %>% make.regression_table(output.path = "tables/default-model_tokens.tex")
model_reasoning_highlight %>% make.regression_table(output.path = "tables/highlight-model_tokens.tex")
model_reasoning_suggestion %>% make.regression_table(output.path = "tables/suggestion-model_tokens.tex")

model_reasoning_default.total %>% make.regression_table(output.path = "tables/default-model_total_tokens.tex")
model_reasoning_highlight.total %>% make.regression_table(output.path = "tables/highlight-model_total_tokens.tex")
model_reasoning_suggestion.total %>% make.regression_table(output.path = "tables/suggestion-model_total_tokens.tex")

emm_default %>% make.emm_table(
  names_from = trial_nudge,
  values_select = c("Abs.", "Pres."),
  caption = "Estimated marginal means (SE) for the default nudge reasoning experiment.",
  output.path = "tables/default-emm-nudge-choice_reasoning.tex",
  label = "default-emm-nudge-choice_reasoning"
)

emm_highlight %>% make.emm_table(
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Estimated marginal means (SE) for the highlight nudge reasoning experiment.",
  output.path = "tables/highlight-emm-nudge-choice_reasoning.tex",
  label = "highlight-emm-nudge-choice_reasoning"
)

emm_suggestion %>% make.emm_table(
  names_from = trial_nudge,
  values_select = c("Early", "Late"),
  caption = "Estimated marginal means (SE) for the suggestion nudge reasoning experiment.",
  output.path = "tables/suggestion-emm-nudge-choice_reasoning.tex",
  label = "suggestion-emm-nudge-choice_reasoning"
)

data_default %>%
  subset(model != "Human") %>%
  droplevels() %>%
  mutate(cost = cost * 100) %>%
  group_by(model, reasoning_effort, trial_nudge) %>%
  summarize(
    avg_cost = mean(cost),
    sd_cost = sd(cost),
    n = n(),
    se_cost = sd_cost / sqrt(n),
    lci.cost = avg_cost - 1.96 * se_cost,
    uci.cost = avg_cost + 1.96 * se_cost
  ) %>%
  ggplot(aes(
    x = trial_nudge,
    y = avg_cost,
    color = model,
    group = interaction(model, reasoning_effort),
    linetype = reasoning_effort
  )) +
  geom_pointrange(
    aes(ymin = lci.cost, ymax = uci.cost),
    size = 0.2
  ) +
  geom_line(
    size = 0.8
  ) +
  xlab("Nudge") +
  ylab("Avg. output cost per turn (cents)") +
  scale_color_npg() +
  scale_linetype_manual(values = c("Minimal" = "dotted", "Low" = "dashed", "Medium" = "solid", "Unknown" = "solid")) +
  theme_nudge() +
  theme(legend.position = "bottom")


data_default %>%
  subset(model != "Human") %>%
  droplevels() %>%
  group_by(model, reasoning_effort, trial_nudge) %>%
  summarize(
    cost = mean(cost) * 100
  ) %>%
  pivot_wider(
    id_cols = c("model", "reasoning_effort"),
    names_from = "trial_nudge",
    values_from = "cost"
  ) %>%
  select(model, reasoning_effort, Abs., Pres.) %>%
  rename(
    Model = model,
    `Reasoning Effort` = reasoning_effort,
    `Avg. Cost (cents) - Nudge Absent` = Abs.,
    `Avg. Cost (cents) - Nudge Present` = Pres.
  ) %>%
  kbl(
    "latex",
    booktabs = TRUE,
    linesep = "",
    digits = 2,
    caption = "Average output cost (in cents) per turn for the default nudge reasoning experiment.",
    escape = FALSE,
    label = "default-avg-cost-per-turn_reasoning",
    position = "!htb"
  ) %>%
  write_lines("tables/default-avg-cost-per-turn_reasoning.tex")

data_highlight %>%
  subset(model != "Human") %>%
  droplevels() %>%
  group_by(model, reasoning_effort, is_nudge_index_optimal) %>%
  summarize(
    cost = mean(cost) * 100
  ) %>%
  pivot_wider(
    id_cols = c("model", "reasoning_effort"),
    names_from = "is_nudge_index_optimal",
    values_from = "cost"
  ) %>%
  select(model, reasoning_effort, Optimal, Suboptimal) %>%
  rename(
    Model = model,
    `Reasoning Effort` = reasoning_effort,
    `Avg. Cost (cents) - Nudge Optimal` = Optimal,
    `Avg. Cost (cents) - Nudge Suboptimal` = Suboptimal
  ) %>%
  kbl(
    "latex",
    booktabs = TRUE,
    linesep = "",
    digits = 2,
    caption = "Average output cost (in cents) per turn for the highlight nudge reasoning experiment.",
    escape = FALSE,
    label = "highlight-avg-cost-per-turn_reasoning",
    position = "!htb"
  ) %>%
  write_lines("tables/highlight-avg-cost-per-turn_reasoning.tex")

data_suggestion %>%
  subset(model != "Human") %>%
  droplevels() %>%
  group_by(model, reasoning_effort, trial_nudge) %>%
  summarize(
    cost = mean(cost) * 100
  ) %>%
  pivot_wider(
    id_cols = c("model", "reasoning_effort"),
    names_from = "trial_nudge",
    values_from = "cost"
  ) %>%
  select(model, reasoning_effort, Abs., Early, Late) %>%
  rename(
    Model = model,
    `Reasoning Effort` = reasoning_effort,
    `Avg. Cost (cents) - Suggestion Absent` = Abs.,
    `Avg. Cost (cents) - Suggestion Early` = Early,
    `Avg. Cost (cents) - Suggestion Late` = Late
  ) %>%
  kbl(
    "latex",
    booktabs = TRUE,
    linesep = "",
    digits = 2,
    caption = "Average output cost (in cents) per turn for the suggestion nudge reasoning experiment.",
    escape = FALSE,
    label = "suggestion-avg-cost-per-turn_reasoning",
    position = "!htb"
  ) %>%
  write_lines("tables/suggestion-avg-cost-per-turn_reasoning.tex")
