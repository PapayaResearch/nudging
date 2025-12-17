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
library(kableExtra)
library(ggtext)
library(directlabels)
library(fixest)
library(modelsummary)

ensure_dirs(c("figures", "tables", "results"))

# ============================================================================
# LOAD DATA
# ============================================================================

data_default <- read.csv("data/data-default.csv") %>%
  preprocess_data(nudge_type = "default") %>%
  mutate(source = source %>% relevel(ref = "Human"))
  
data_highlight <- read.csv("data/data-highlight.csv") %>%
  preprocess_data(nudge_type = "highlight") %>%
  mutate(source = source %>% relevel(ref = "Human"))

data_suggestion <- read.csv("data/data-suggestion.csv") %>%
  preprocess_data(nudge_type = "suggestion") %>%
  mutate(source = source %>% relevel(ref = "Human"))

data_optimal <- read.csv("data/data-optimal.csv") %>%
  preprocess_data(nudge_type = "optimal") %>%
  mutate(source = source %>% relevel(ref = "Human"))

# ============================================================================
# DEFAULT EXPERIMENT
# ============================================================================

data_default_prep <- data_default %>%
  mutate(chose_nudge = recode(chose_nudge, "True" = 1, "False" = 0))

model_default <- feglm(
  chose_nudge ~ source * method * trial_nudge,
  data = data_default_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm.gridshape <- feglm(
  chose_nudge ~ source * trial_nudge * grid_shape,
  data = data_default_prep %>% subset(method == "Base"),
  family = binomial,
  cluster = ~ participant_id
) %>% get_marginal_effects(
  ~ source | trial_nudge + grid_shape,
  data = data_default_prep %>% subset(method == "Base")
)

emm.gridshape <- emm.gridshape %>%
  as_tibble() %>%
  left_join(
    emm.gridshape %>%
      compute_human_contrasts(by_vars = c("trial_nudge", "grid_shape")) %>%
      mutate(
        contrast = contrast %>% str_replace_all(" / Human", ""),
        source = contrast %>% str_trim()
      ) %>% select(source, trial_nudge, grid_shape, p.value),
    by = c("source", "trial_nudge", "grid_shape")
  ) %>%
  add_significance_stars(p_col = "p.value") %>%
  rename(contrast_p_value = p.value)

p.gridshape <- emm.gridshape %>%
  as_tibble() %>%
  ggplot(
    aes(
      reorder(source, ifelse(trial_nudge == "Pres.", prob, 0) + (source == "Human")), prob, color = grid_shape)
    ) +
    geom_pointrange(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      size = 0.2,
      position = position_dodge(width = 0.5)
    ) +
    geom_hline(
      aes(yintercept = prob, color = grid_shape),
      data = emm.gridshape %>% subset(source == "Human"),
      linetype = "dotted",
      linewidth = 0.4
    ) +
    facet_wrap(~ trial_nudge) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      expand = expansion(mult = c(0.1, 0.18))
    ) +
    scale_color_atlassian() +
    coord_flip() +
    xlab("Model") +
    ylab("P(Choose Nudge)") +
    guides(color = guide_legend(title = "Grid Shape")) +
    theme_nudge()

p.gridshape %>%
  ggsave(
    filename = "figures/default-marginals-by-gridshape.pdf",
    plot = .,
    width = 8,
    height = 6
  )

emm_default <- get_marginal_effects(
  model_default,
  ~ source + method | trial_nudge,
  data = data_default_prep
)

contrasts_default <- emm_default %>%
  compute_human_contrasts(by_vars = "trial_nudge") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human", ""),
    source = contrast %>% str_trim()
  ) %>% select(source, method, trial_nudge, p.value)

emm_default_diff <- emm_default %>%
  regrid() %>%
  contrast(method = "revpairwise", by = c("source", "method")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(trial_nudge = contrast)

emm_default <- emm_default %>% as_tibble() %>%
  left_join(
    contrasts_default,
    by = c("source", "method", "trial_nudge")
  ) %>% rename(contrast_p_value = p.value)


p.idiosyncracy <- data_default_prep %>%
  subset(trial_nudge == "Pres." & method == "Base") %>%
  subset(
    source %in% c(
      "Human",
      "GPT-5",
      "Gemini 2.5 Pro",
      "Claude 4.5 Sonnet",
      "o3"
    )
  ) %>%
  mutate(
    accepted_default = recode(accepted_default, "True" = 1, "False" = 0)
  ) %>%
  group_by(
    source,
    idiosyncracy_bin = cut(idiosyncracy, breaks = seq(0, 32, by = 4), include.lowest = TRUE)
  ) %>%
  summarize(
    prob_accepted_default = mean(accepted_default, na.rm = TRUE),
    prob_chose_nudge = mean(chose_nudge, na.rm = TRUE),
    prob_rejected_default_and_chose_nudge = mean((!accepted_default) & chose_nudge, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(
      x = idiosyncracy_bin,
      y = prob_rejected_default_and_chose_nudge,
      color = source,
      group = source
    )
  ) +
  geom_point() +
  geom_line() +
  directlabels::geom_dl(
    aes(label = source),
    method = list("last.qp", cex = 0.8, dl.trans(x = x + 0.2))
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.28))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_color_cosmic() +
  xlab("Idiosyncracy Bin") +
  ylab("P(Reject Then Choose Nudge)") +
  theme_nudge() +
  theme(legend.position = "none")

p.idiosyncracy %>%
  ggsave(
    filename = "figures/default-idiosyncracy-effect.pdf",
    plot = .,
    width = 6.8,
    height = 4
  )

# ============================================================================
# SUGGESTION EXPERIMENT
# ============================================================================

data_suggestion_prep <- data_suggestion %>%
  filter(trial_nudge != "Abs.") %>%
  droplevels() %>%
  mutate(chose_nudge = recode(chose_nudge, "True" = 1, "False" = 0))

p.switch <- data_suggestion_prep %>%
  subset((trial_nudge == "Late") & (selected_option != first_selected_option) & (source != "Human")) %>%
  group_by(source) %>%
  summarize(
    bad_switch_rate = mean(value_first_option_selected > value_final_option_selected),
    n = n(),
    lower.ci = binom.test(sum(value_first_option_selected > value_final_option_selected), n)$conf.int[1],
    upper.ci = binom.test(sum(value_first_option_selected > value_final_option_selected), n)$conf.int[2]
  ) %>%
  ggplot(
    aes(
      reorder(source, bad_switch_rate),
      bad_switch_rate
    )
  ) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = lower.ci,
      ymax = upper.ci
    ),
    width = 0.2
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.0, 0.05))
  ) +
  xlab("Source") +
  ylab("% Switched to Less Optimal Suggestion") +
  theme_nudge() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.switch %>%
  ggsave(
    filename = "figures/suggestion-bad-switch-rate.pdf",
    plot = .,
    width = 6,
    height = 4
  )

model_suggestion <- feglm(
  chose_nudge ~ source * trial_nudge * method,
  data = data_suggestion_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm_suggestion <- get_marginal_effects(
  model_suggestion,
  ~ source + method | trial_nudge,
  data = data_suggestion_prep
)

contrasts_suggestion <- emm_suggestion %>%
  compute_human_contrasts(by_vars = "trial_nudge") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human", ""),
    source = contrast %>% str_trim()
  ) %>% select(source, method, trial_nudge, p.value)

emm_suggestion_diff <- emm_suggestion %>%
  regrid() %>%
  contrast(method = "pairwise", by = c("source", "method")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(trial_nudge = contrast)

emm_suggestion <- emm_suggestion %>% as_tibble() %>%
  left_join(
    contrasts_suggestion,
    by = c("source", "method", "trial_nudge")
  ) %>% rename(contrast_p_value = p.value)

# ============================================================================
# HIGHLIGHT EXPERIMENT
# ============================================================================

data_highlight_prep <- data_highlight %>%
  mutate(is_first_index_nudged = recode(is_first_index_nudged, "True" = 1, "False" = 0)) %>%
  filter(trial_nudge == "Pres.")

model_highlight <- feglm(
  is_first_index_nudged ~ source * method * is_nudge_index_optimal,
  data = data_highlight_prep,
  family = binomial,
  cluster = ~ participant_id
)

emm_highlight <- get_marginal_effects(
  model_highlight,
  ~ source + method | is_nudge_index_optimal,
  data = data_highlight_prep
)

data_highlight_prep.trial_nudge <- data_highlight %>%
  mutate(is_first_index_nudged = recode(is_first_index_nudged, "True" = 1, "False" = 0))

emm_highlight.trial_nudge <- feglm(
  is_first_index_nudged ~ source * method * trial_nudge,
  data = data_highlight_prep.trial_nudge,
  family = binomial,
  cluster = ~ participant_id
) %>% get_marginal_effects(
  ~ source + method | trial_nudge,
  data = data_highlight_prep.trial_nudge
)

p.highlight.trial_nudge <- emm_highlight.trial_nudge %>%
  as_tibble() %>%
  ggplot(aes(
    reorder(source, ifelse(trial_nudge == "Pres.", prob, 0) + (source == "Human")), prob, color = trial_nudge
  )) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_highlight.trial_nudge %>% as_tibble() %>% subset(source == "Human" & trial_nudge == "Abs.") %>% pull(prob),
    linetype = "dashed",
    color = pal_aaas()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_highlight.trial_nudge %>% as_tibble() %>% subset(source == "Human" & trial_nudge == "Pres.") %>% pull(prob),
    linetype = "dashed",
    color = pal_aaas()(2)[2]
  ) +
  facet_wrap(~ method) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_color_aaas() +
  coord_flip() +
  xlab("Model") +
  ylab("P(Choose Nudge)") +
  guides(color = guide_legend(title = "Highlight Absent/Present")) +
  theme_nudge()

p.highlight.trial_nudge %>%
  ggsave(
    filename = "figures/highlight-marginals-by-trial-nudge.pdf",
    plot = .,
    width = 8,
    height = 4
  )

contrasts_highlight <- emm_highlight %>%
  compute_human_contrasts(by_vars = "is_nudge_index_optimal") %>%
  mutate(
    contrast = contrast %>% str_replace_all(" / Human", ""),
    source = contrast %>% str_trim()
  ) %>% select(source, method, is_nudge_index_optimal, p.value)

emm_highlight_diff <- emm_highlight %>%
  regrid() %>%
  contrast(method = "revpairwise", by = c("source", "method")) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  rename(is_nudge_index_optimal = contrast)

emm_highlight <- emm_highlight %>% as_tibble() %>%
  left_join(
    contrasts_highlight,
    by = c("source", "method", "is_nudge_index_optimal")
  ) %>% rename(contrast_p_value = p.value)

# ============================================================================
# INFORMATION ACQUISITION
# ============================================================================

ks_default <- calculate_ks_stats(data_default, "trial_nudge == 'Abs.'") %>%
  mutate(experiment = "Default")

ks_highlight <- calculate_ks_stats(data_highlight, "trial_nudge == 'Abs.'") %>%
  mutate(experiment = "Highlight")

ks_suggestion <- calculate_ks_stats(data_suggestion, "trial_nudge == 'Abs.'") %>%
  mutate(experiment = "Suggestion")

ks_optimal <- calculate_ks_stats(data_optimal, group_vars = c("source", "method")) %>%
  mutate(experiment = "Optimal")

ks_combined <- bind_rows(ks_default, ks_highlight, ks_suggestion, ks_optimal) %>%
  mutate(experiment = factor(experiment, levels = c("Default", "Suggestion", "Highlight", "Optimal")))

# ============================================================================
# EARNINGS
# ============================================================================

earnings_default <- analyze_earnings(data_default, "trial_nudge")
emm_earnings_default <- earnings_default$emm

earnings_highlight <- analyze_earnings(data_highlight, "trial_nudge")
emm_earnings_highlight <- earnings_highlight$emm

earnings_suggestion <- analyze_earnings(data_suggestion, "trial_nudge")
emm_earnings_suggestion <- earnings_suggestion$emm

earnings_optimal_model <- feols(
  total_points ~ source * nudge_type,
  data = data_optimal,
  cluster = ~ participant_id
)

emm_earnings_optimal <- earnings_optimal_model %>% emmeans(
  ~ source | nudge_type,
  data = data_optimal
)

earnings_combined <- bind_rows(
  combine_earnings_data(
    emm_earnings_default,
    create_human_contrasts(emm_earnings_default),
    "Default"
  ),
  combine_earnings_data(
    emm_earnings_highlight,
    create_human_contrasts(emm_earnings_highlight),
    "Highlight"
  ),
  combine_earnings_data(
    emm_earnings_suggestion,
    create_human_contrasts(emm_earnings_suggestion),
    "Suggestion"
  )
) %>%
  rename(condition = trial_nudge) %>%
  bind_rows(
    combine_earnings_data(
      emm_earnings_optimal %>% as_tibble() %>% rename(trial_nudge = nudge_type),
      create_human_contrasts(emm_earnings_optimal) %>% as_tibble() %>% rename(trial_nudge = nudge_type),
      "Optimal"
    ) %>%
      rename(condition = trial_nudge)
  )

# ============================================================================
# OTHER PREPROCESSING
# ============================================================================

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

p.default <- emm_default %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(source, ifelse(trial_nudge == "Pres.", prob, 0) + (source == "Human")), prob, color = trial_nudge)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_default %>% subset(source == "Human" & trial_nudge == "Abs.") %>% pull(prob),
    linetype = "dashed",
    color = pal_aaas()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_default %>% subset(source == "Human" & trial_nudge == "Pres.") %>% pull(prob),
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
  facet_wrap(~ method) +
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
  theme_nudge()

p.default %>%
  ggsave(
    filename = "figures/default-marginals.pdf",
    plot = .,
    width = 8,
    height = 4
  )


p.suggestion <- emm_suggestion %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(source, ifelse(trial_nudge == "Early", prob, 0) + (source == "Human")), prob, color = trial_nudge)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_suggestion %>% subset(source == "Human" & trial_nudge == "Early") %>% pull(prob),
    linetype = "dashed",
    color = pal_primer()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_suggestion %>% subset(source == "Human" & trial_nudge == "Late") %>% pull(prob),
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
  facet_wrap(~ method) +
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
  theme_nudge()

p.suggestion %>%
  ggsave(
    filename = "figures/suggestion-marginals.pdf",
    plot = .,
    width = 8,
    height = 4
  )


p.highlight <- emm_highlight %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
  ggplot(aes(reorder(source, ifelse(is_nudge_index_optimal == "Suboptimal", prob, 0) + (source == "Human")), prob, color = is_nudge_index_optimal)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3
  ) +
  geom_hline(
    yintercept = emm_highlight %>% subset(source == "Human" & is_nudge_index_optimal == "Optimal") %>% pull(prob),
    linetype = "dashed",
    color = pal_cosmic()(2)[1]
  ) +
  geom_hline(
    yintercept = emm_highlight %>% subset(source == "Human" & is_nudge_index_optimal == "Suboptimal") %>% pull(prob),
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
  facet_wrap(~ method) +
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
  theme_nudge()

p.highlight %>%
  ggsave(
    filename = "figures/highlight-marginals.pdf",
    plot = .,
    width = 8,
    height = 4
  )


p.nudge <- (p.default / p.suggestion / p.highlight) +
  plot_layout(
    guides = "collect",
    axes = "collect"
  ) &
  theme(legend.position = "bottom", legend.direction = "vertical")
 
p.nudge %>% 
  ggsave(
    filename = "figures/nudge-marginals.pdf",
    plot = .,
    width = 10,
    height = 10
  )


diffs_combined %>%
  ggplot(aes(reorder(source, ifelse(nudge_type == "Suggestion", estimate, 0) + (source == "Human")), estimate, color = contrast)) +
  geom_hline(
    aes(yintercept = estimate, color = contrast),
    data = diffs_combined %>%
      subset(source == "Human") %>%
      select(contrast, estimate, nudge_type),
    linetype = "dashed"
  ) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 0.3,
    position = position_dodge(width = 0.5)
  ) +
  ggh4x::facet_grid2(
    method ~ nudge_type,
    scales = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(-0.5, 1, by = 0.25),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_color_uchicago() +
  coord_flip() +
  xlab("Model") +
  ylab("∆P(Choose Nudge)") +
  guides(color = guide_legend(title = "Difference")) +
  theme_nudge()


plot.ks <- ks_combined %>%
  add_significance_stars(p_col = "ks_p") %>%
  ggplot(aes(reorder(source, ks_stat), ks_stat, color = method, fill = method)) +
  geom_bar(
    stat = "identity",
    position = position_dodge2(width = 0.7, preserve = "single"),
    width = 0.6,
    color = "black",
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      y = ks_stat + 0.12,
      label = sig_stars
    ),
    size = 4,
    position = position_dodge2(width = 0.7, preserve = "single"),
    vjust = 0.75,
    hjust = 0.5,
    show.legend = FALSE
  ) +
  facet_wrap(~ experiment, nrow = 1) +
  scale_y_continuous(
    limits = c(0, 1.1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_color_atlassian() +
  scale_fill_atlassian() +
  coord_flip() +
  xlab("Model") +
  ylab("KS Statistic") +
  guides(fill = guide_legend(title = "Method")) +
  theme_nudge()

plot.ks %>%
  ggsave(
    filename = "figures/ks-stats.pdf",
    plot = .,
    width = 10,
    height = 6
  )

plot.earnings <- earnings_combined %>%
  mutate(
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    )
  ) %>%
  ggplot(aes(reorder(source, emmean + ((source == "Human") * 200)), emmean, color = experiment)) +
  geom_hline(
    aes(yintercept = emmean, linetype = source),
    data = earnings_combined %>%
      subset(source == "Human") %>%
      select(condition, emmean, experiment, source)
  ) +
  geom_hline(
    aes(yintercept = RANDOM_PAYOFF, linetype = "Random"),
    color = "pink",
    linewidth = 1,
    alpha = 0.6
  ) +
  geom_hline(
    aes(yintercept = MAXIMUM_PAYOFF, linetype = "Maximum"),
    color = "lightblue",
    linewidth = 1,
    alpha = 0.6
  ) +
  geom_pointrange(
    aes(ymin = lower.CL, ymax = upper.CL),
    size = 0.3,
    position = position_dodge(width = 0.5),
    show.legend = FALSE
  ) +
  facet_wrap(~ interaction(experiment, condition, sep = ": ", lex.order = TRUE), nrow = 2) +
  scale_y_continuous(
    limits = c(100, 200),
    breaks = seq(120, 180, by = 30),
    expand = expansion(mult = c(0.1, 0.18))
  ) +
  scale_color_d3() +
  scale_linetype_manual(
    values = c("Human" = "dashed", "Random" = "solid", "Maximum" = "solid"),
    NULL
  ) +
  coord_flip() +
  xlab("Model") +
  ylab("Estimated Earnings") +
  theme_nudge() +
  theme(strip.text = element_text(size = 10))

plot.earnings %>%
  ggsave(
    filename = "figures/earnings-estimates.pdf",
    plot = .,
    width = 10,
    height = 6
  )

data_all <- bind_rows(
  data_default %>%
    select(source, method, trial_nudge, n_uncovered) %>%
    mutate(experiment = "Default"),
  data_suggestion %>%
    select(source, method, trial_nudge, n_uncovered) %>%
    mutate(experiment = "Suggestion"),
  data_highlight %>%
    select(source, method, trial_nudge, n_uncovered) %>%
    mutate(experiment = "Highlight")
)

p.uncovered <- data_all %>%
  subset(trial_nudge == "Abs.") %>%
  mutate(
    source_parent = case_when(
      source == "Human" ~ " ",
      str_detect(as.character(source), "o3") ~ "o-Series",
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
    ) %>% str_wrap(4)
  ) %>%
  ggplot(aes(x = n_uncovered, fill = method)) +
  geom_density(
    alpha = 0.6
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = c(0, 0),
    breaks = seq(0, 20, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_prompt() +
  xlab("Number of Items Uncovered") +
  ylab("") +
  guides(fill = guide_legend(title = "Method")) +
  ggh4x::facet_nested(
    source_parent + source_child ~ experiment + method,
    scales = "free_y",
    switch = "y",
    nest_line = element_line(linewidth = 0.5)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(size = 8, angle = 30),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

p.uncovered %>%
  ggsave(
    filename = "figures/uncovered_items_density_combined.pdf",
    plot = .,
    width = 10,
    height = 8
  )

# ============================================================================
# TABLES
# ============================================================================

earnings_default$model %>% make.regression_table(output.path = "tables/default-model_earnings.tex")
earnings_suggestion$model %>% make.regression_table(output.path = "tables/suggestion-model_earnings.tex")
earnings_highlight$model %>% make.regression_table(output.path = "tables/highlight-model_earnings.tex")

earnings_combined.table <- earnings_combined %>%
  add_significance_stars(p_col = "contrast_p_value") %>%
    mutate(
      EMM = paste0(round(emmean, 1), "$^{", sig_stars, "}$", " (", round(SE, 1), ")")
    )

earnings_combined.table %>%
    kbl("markdown") %>%
    write_lines("tables/earnings-emm-table.md")

earnings_combined.table %>%
  pivot_wider(
    names_from = c("experiment", "condition"),
    values_from = EMM,
    id_cols = c("source"),
    names_sep = " / "
  ) %>%
  kbl(
    "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Estimated marginal mean earnings (SE) across all models and conditions.",
    linesep = "",
    position = "!htb",
    label = "earnings-emm-table"
  ) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c(" " = 1, "Estimated Earnings (SE)" = 8)) %>%
  write_lines("tables/earnings-emm-table.tex")


ks_combined.table <- ks_combined %>%
  add_significance_stars(p_col = "ks_p") %>%
    mutate(
      D = paste0(round(ks_stat, 2)) %>%
        ifelse(is.na(sig_stars), ., paste0(., "$^{", sig_stars, "}$"))
    ) %>%
    arrange(source, method, experiment)

ks_combined.table %>%
    kable("markdown") %>%
    write_lines("tables/ks-statistics-table.md")

ks_combined.table %>%
    pivot_wider(
      names_from = c("experiment"),
      values_from = D,
      id_cols = c("source", "method"),
      names_sep = " --- "
    ) %>%
    kbl(
      "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = "KS statistics across all models and experiments.",
      linesep = "",
      position = "!htb",
      label = "ks-statistics-table"
    ) %>%
    add_header_above(c(" " = 2, "KS Statistic" = 4)) %>%
    write_lines("tables/ks-statistics-table.tex")


emm_default %>% make.emm_table(
  names_from = trial_nudge,
  values_select = c("Abs.", "Pres."),
  caption = "Estimated marginal means (SE) for the default nudge experiment.",
  output.path = "tables/default-emm-nudge-choice.tex",
  id_cols = c("source", "method"),
  rename_cols = c(Model = "source", Method = "method"),
  label = "default-emm-nudge-choice"
)

emm_highlight %>% make.emm_table(
  names_from = is_nudge_index_optimal,
  values_select = c("Optimal", "Suboptimal"),
  caption = "Estimated marginal means (SE) for the highlight nudge experiment.",
  output.path = "tables/highlight-emm-nudge-choice.tex",
  id_cols = c("source", "method"),
  rename_cols = c(Model = "source", Method = "method"),
  label = "highlight-emm-nudge-choice"
)

emm_suggestion %>% make.emm_table(
  names_from = trial_nudge,
  values_select = c("Early", "Late"),
  caption = "Estimated marginal means (SE) for the suggestion nudge experiment.",
  output.path = "tables/suggestion-emm-nudge-choice.tex",
  id_cols = c("source", "method"),
  rename_cols = c(Model = "source", Method = "method"),
  label = "suggestion-emm-nudge-choice"
)
