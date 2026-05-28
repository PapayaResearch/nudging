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
    ylab("P(Follow Nudge)") +
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
  ylab("P(Reject Then Follow Nudge)") +
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
  ylab("P(Follow Nudge)") +
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

annotate_ks_results <- function(ks_results, experiment_name) {
  list(
    main = ks_results$main %>%
      mutate(experiment = experiment_name),
    robustness = ks_results$robustness %>%
      mutate(experiment = experiment_name)
  )
}

ks_default <- calculate_ks_stats(data_default, "trial_nudge == 'Abs.'") %>%
  annotate_ks_results("Default")

ks_highlight <- calculate_ks_stats(data_highlight, "trial_nudge == 'Abs.'") %>%
  annotate_ks_results("Highlight")

ks_suggestion <- calculate_ks_stats(data_suggestion, "trial_nudge == 'Abs.'") %>%
  annotate_ks_results("Suggestion")

ks_optimal <- calculate_ks_stats(data_optimal, group_vars = c("source", "method")) %>%
  annotate_ks_results("Optimal")

ks_combined <- bind_rows(
  ks_default$main,
  ks_highlight$main,
  ks_suggestion$main,
  ks_optimal$main
) %>%
  mutate(experiment = factor(experiment, levels = c("Default", "Suggestion", "Highlight", "Optimal")))

ks_combined_robustness <- bind_rows(
  ks_default$robustness,
  ks_highlight$robustness,
  ks_suggestion$robustness,
  ks_optimal$robustness
) %>%
  mutate(experiment = factor(experiment, levels = c("Default", "Suggestion", "Highlight", "Optimal")))

ks_human_baseline <- bind_rows(
  calculate_human_split_half_ks_summary(data_default, "trial_nudge == 'Abs.'") %>%
    mutate(experiment = "Default"),
  calculate_human_split_half_ks_summary(data_highlight, "trial_nudge == 'Abs.'") %>%
    mutate(experiment = "Highlight"),
  calculate_human_split_half_ks_summary(data_suggestion, "trial_nudge == 'Abs.'") %>%
    mutate(experiment = "Suggestion"),
  calculate_human_split_half_ks_summary(data_optimal) %>%
    mutate(experiment = "Optimal")
) %>%
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
  ylab("P(Follow Nudge)") +
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
  ylab("P(Follow Nudge)") +
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
  ylab("P(Follow Nudge)") +
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
  ylab("∆P(Follow Nudge)") +
  guides(color = guide_legend(title = "Difference")) +
  theme_nudge()

make_ks_plot <- function(ks_data, ks_human_baseline) {
  ks_data %>%
    add_significance_stars(p_col = "ks_p") %>%
    ggplot(aes(reorder(source, ks_stat), ks_stat, color = method, fill = method)) +
    geom_rect(
      data = ks_human_baseline,
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = ks_stat_ci_lower,
        ymax = ks_stat_ci_upper
      ),
      fill = "grey50",
      alpha = 0.12,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_hline(
      data = ks_human_baseline,
      aes(yintercept = ks_stat_mean, linetype = "Human split-half KS mean"),
      color = "black",
      linewidth = 0.5,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
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
    scale_linetype_manual(values = c("Human split-half KS mean" = "dashed")) +
    coord_flip() +
    xlab("Model") +
    ylab("KS Statistic") +
    guides(
      color = "none",
      fill = guide_legend(title = "Method", order = 1),
      linetype = guide_legend(
        title = NULL,
        order = 2,
        override.aes = list(color = "black", linewidth = 0.5)
      )
    ) +
    theme_nudge()
}

plot.ks <- make_ks_plot(ks_combined, ks_human_baseline)
plot.ks.robustness <- make_ks_plot(ks_combined_robustness, ks_human_baseline)

plot.ks %>%
  ggsave(
    filename = "figures/ks-stats.pdf",
    plot = .,
    width = 10,
    height = 6
  )

plot.ks.robustness %>%
  ggsave(
    filename = "figures/ks-stats-robustness-simulated-p-value.pdf",
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

compute_cohens_d <- function(x, y) {
  pooled_sd <- sqrt(
    (
      ((length(x) - 1) * stats::sd(x)^2) +
      ((length(y) - 1) * stats::sd(y)^2)
    ) / (length(x) + length(y) - 2)
  )

  ifelse(pooled_sd == 0, NA_real_, (mean(x) - mean(y)) / pooled_sd)
}

compute_sign_flip_p <- function(diff) {
  diff <- diff[!is.na(diff) & diff != 0]

  if (length(diff) == 0) {
    return(NA_real_)
  }

  observed <- mean(diff)
  null_signs <- expand.grid(rep(list(c(-1, 1)), length(diff)))
  null_means <- as.vector(as.matrix(null_signs) %*% abs(diff) / length(diff))

  mean(null_means >= observed)
}

make_gap_summary <- function(
  data,
  value_var,
  group_vars,
  experiment_name
) {
  if (is.null(group_vars)) {
    data <- data %>%
      mutate(.gap_group = "All")
    group_vars <- ".gap_group"
  }

  data %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(
      function(df, keys) {
        human_values <- df %>%
          subset(source == "Human") %>%
          pull(!!sym(value_var))

        df %>%
          subset(source != "Human") %>%
          group_by(source) %>%
          summarize(
            gap = abs(compute_cohens_d(.data[[value_var]], human_values)),
            .groups = "drop"
          )
      }
    ) %>%
    ungroup() %>%
    mutate(experiment = experiment_name) %>%
    group_by(source, experiment) %>%
    summarize(
      gap = mean(gap, na.rm = TRUE),
      .groups = "drop"
    )
}

outcome_gap_combined <- bind_rows(
  make_gap_summary(
    data_default,
    "total_points",
    "trial_nudge",
    "Default"
  ),
  make_gap_summary(
    data_suggestion,
    "total_points",
    "trial_nudge",
    "Suggestion"
  ),
  make_gap_summary(
    data_highlight,
    "total_points",
    "trial_nudge",
    "Highlight"
  ),
  make_gap_summary(
    data_optimal,
    "total_points",
    "nudge_type",
    "Optimal"
  )
) %>%
  rename(outcome_gap = gap) %>%
  mutate(
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    )
  )

intervention_gap_combined <- bind_rows(
  make_gap_summary(
    data_default_prep,
    "chose_nudge",
    "trial_nudge",
    "Default"
  ),
  make_gap_summary(
    data_suggestion_prep,
    "chose_nudge",
    "trial_nudge",
    "Suggestion"
  ),
  make_gap_summary(
    data_highlight_prep,
    "is_first_index_nudged",
    "is_nudge_index_optimal",
    "Highlight"
  )
) %>%
  rename(intervention_gap = gap) %>%
  mutate(
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight")
    )
  )

strategy_gap_combined <- bind_rows(
  make_gap_summary(
    data_default %>% subset(trial_nudge == "Abs."),
    "n_uncovered",
    NULL,
    "Default"
  ),
  make_gap_summary(
    data_suggestion %>% subset(trial_nudge == "Abs."),
    "n_uncovered",
    NULL,
    "Suggestion"
  ),
  make_gap_summary(
    data_highlight %>% subset(trial_nudge == "Abs."),
    "n_uncovered",
    NULL,
    "Highlight"
  ),
  make_gap_summary(
    data_optimal,
    "n_uncovered",
    NULL,
    "Optimal"
  )
) %>%
  rename(strategy_gap = gap) %>%
  mutate(
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    )
  )

process_outcome_intervention <- outcome_gap_combined %>%
  subset(experiment != "Optimal") %>%
  left_join(
    intervention_gap_combined,
    by = c("source", "experiment")
  ) %>%
  transmute(
    source = source,
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    ),
    outcome_gap = outcome_gap,
    process_gap = intervention_gap,
    metric = "Intervention-Sensitivity Gap",
    color_group = "intervention"
  )

process_outcome_strategy <- outcome_gap_combined %>%
  left_join(
    strategy_gap_combined,
    by = c("source", "experiment")
  ) %>%
  transmute(
    source = source,
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    ),
    outcome_gap = outcome_gap,
    process_gap = strategy_gap,
    metric = "Strategy Divergence from Human",
    color_group = "strategy"
  )

process_outcome_blank <- tibble(
  source = NA_character_,
  experiment = factor(
    "Optimal",
    levels = c("Default", "Suggestion", "Highlight", "Optimal")
  ),
  outcome_gap = NA_real_,
  process_gap = NA_real_,
  metric = "Intervention-Sensitivity Gap",
  color_group = "intervention"
)

process_outcome_combined <- bind_rows(
  process_outcome_intervention,
  process_outcome_strategy,
  process_outcome_blank
) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Intervention-Sensitivity Gap",
        "Strategy Divergence from Human"
      )
    )
  ) %>%
  ungroup()

process_outcome_perm_test <- process_outcome_combined %>%
  mutate(diff = process_gap - outcome_gap) %>%
  group_by(metric, experiment) %>%
  summarize(
    n_positive = sum(diff > 0, na.rm = TRUE),
    n_nonzero = sum((diff != 0) & !is.na(diff)),
    p_value = compute_sign_flip_p(diff),
    .groups = "drop"
  ) %>%
  mutate(
    label = ifelse(
      is.na(p_value),
      NA,
      "Perm. test p = %s" %>% sprintf(format.pval(p_value, digits = 2, eps = 0.0001))
    ),
    x = 1.5,
    y = 0.2
  )

process_outcome_labels <- process_outcome_combined %>%
  subset(!is.na(source) & !is.na(outcome_gap) & !is.na(process_gap)) %>%
  group_by(metric, experiment) %>%
  reframe(
    bind_rows(
      slice_min(cur_data(), outcome_gap, n = 1, with_ties = FALSE),
      slice_max(cur_data(), outcome_gap, n = 1, with_ties = FALSE),
      slice_min(cur_data(), process_gap, n = 1, with_ties = FALSE),
      slice_max(cur_data(), process_gap, n = 1, with_ties = FALSE)
    ) %>%
      distinct(source, .keep_all = TRUE)
  ) %>%
  ungroup()

plot.process_outcome <- process_outcome_combined %>%
  ggplot(aes(outcome_gap, process_gap, color = color_group)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dotted",
    color = "gray55",
    linewidth = 0.6
  ) +
  geom_point(
    size = 3,
    alpha = 0.6
  ) +
  geom_text(
    data = process_outcome_labels,
    aes(label = source),
    hjust = -0.15,
    vjust = 0.2,
    size = 3,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  geom_label(
    data = process_outcome_perm_test,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0.5,
    vjust = 1,
    size = 3,
    linewidth = 0.25,
    fill = "white",
    color = "black"
  ) +
  facet_grid(
    metric ~ experiment,
    drop = FALSE
  ) +
  scale_color_manual(
    values = c(
      "intervention" = "#2F5D62",
      "strategy" = "#8C3B3B"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 2.08),
    breaks = seq(0, 1.5, by = 0.5),
    expand = expansion(mult = c(0.04, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 2.08),
    breaks = seq(0, 1.5, by = 0.5),
    expand = expansion(mult = c(0.04, 0))
  ) +
  xlab(expression("Earnings Gap from Human (" * "|" * "Cohen's " * italic(d) * "|" * ")")) +
  ylab(expression("Process Gap from Human (" * "|" * "Cohen's " * italic(d) * "|" * ")")) +
  theme_nudge() +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(8, 18, 8, 8)
  )

plot.process_outcome %>%
  ggsave(
    filename = "figures/outcome-process-alignment.pdf",
    plot = .,
    width = 14,
    height = 8
  )

 baseline_conditions <- tribble(
  ~experiment, ~baseline_condition,
  "Default", "Abs.",
  "Suggestion", "Abs.",
  "Highlight", "Abs.",
  "Optimal", "Random"
)

condition_labels <- tribble(
  ~experiment, ~condition, ~contrast_label,
  "Default", "Pres.", "Present default vs absent",
  "Suggestion", "Early", "Early suggestion vs absent",
  "Suggestion", "Late", "Late suggestion vs absent",
  "Highlight", "Pres.", "Highlight present vs absent",
  "Optimal", "Extreme", "Extreme pre-reveal vs random",
  "Optimal", "Optimal", "Optimal pre-reveal vs random"
)

earnings_deltas <- earnings_combined %>%
  left_join(
    baseline_conditions,
    by = "experiment"
  ) %>%
  left_join(
    condition_labels,
    by = c("experiment", "condition")
  ) %>%
  group_by(source, experiment) %>%
  mutate(
    baseline_emmean = emmean[condition == baseline_condition]
  ) %>%
  ungroup() %>%
  subset(!is.na(contrast_label)) %>%
  mutate(
    earnings_delta = emmean - baseline_emmean,
    experiment = factor(
      experiment,
      levels = c("Default", "Suggestion", "Highlight", "Optimal")
    )
  )

human_earnings_deltas <- earnings_deltas %>%
  subset(source == "Human") %>%
  select(experiment, contrast_label, human_earnings_delta = earnings_delta)

plot.earnings_deltas <- earnings_deltas %>%
  subset(source != "Human") %>%
  left_join(
    human_earnings_deltas,
    by = c("experiment", "contrast_label")
  ) %>%
  mutate(
    source = fct_reorder(source, earnings_delta, .desc = FALSE),
    contrast_label = factor(
      contrast_label,
      levels = c(
        "Present default vs absent",
        "Early suggestion vs absent",
        "Late suggestion vs absent",
        "Highlight present vs absent",
        "Extreme pre-reveal vs random",
        "Optimal pre-reveal vs random"
      )
    )
  ) %>%
  ggplot(aes(source, earnings_delta, color = experiment)) +
  geom_hline(
    aes(yintercept = 0, linetype = "No earnings change"),
    color = "gray70",
    linewidth = 0.4,
    show.legend = TRUE
  ) +
  geom_hline(
    aes(yintercept = human_earnings_delta, linetype = "Human earnings change"),
    color = "black",
    linewidth = 0.5,
    show.legend = TRUE
  ) +
  geom_pointrange(
    aes(ymin = lower.CL - baseline_emmean, ymax = upper.CL - baseline_emmean),
    size = 0.3,
    show.legend = FALSE
  ) +
  facet_wrap(~ interaction(experiment, contrast_label, sep = ": ", lex.order = TRUE), scales = "free_x", nrow = 2) +
  scale_color_d3() +
  scale_linetype_manual(
    values = c(
      "No earnings change" = "solid",
      "Human earnings change" = "dashed"
    ),
    name = NULL
  ) +
  coord_flip() +
  xlab("Model") +
  ylab("Estimated Earnings Change from Baseline") +
  guides(color = guide_legend(title = "Experiment")) +
  theme_nudge() +
  theme(plot.subtitle = element_text(size = 10, hjust = 0)) +
  theme(strip.text = element_text(size = 10))

plot.earnings_deltas %>%
  ggsave(
    filename = "figures/earnings-deltas.pdf",
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

make_ks_table_data <- function(ks_data) {
  ks_data %>%
    add_significance_stars(p_col = "ks_p") %>%
    mutate(
      D = paste0(round(ks_stat, 2)) %>%
        ifelse(is.na(sig_stars), ., paste0(., "$^{", sig_stars, "}$"))
    ) %>%
    arrange(source, method, experiment)
}

ks_combined.table <- make_ks_table_data(ks_combined)
ks_combined_robustness.table <- make_ks_table_data(ks_combined_robustness)

ks_combined.table %>%
  kable("markdown") %>%
  write_lines("tables/ks-statistics-table.md")

ks_combined_robustness.table %>%
  kable("markdown") %>%
  write_lines("tables/ks-statistics-table-robustness-simulated-p-value.md")

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

ks_combined_robustness.table %>%
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
    caption = "KS statistics across all models and experiments. Robustness check using simulated p-values.",
    linesep = "",
    position = "!htb",
    label = "ks-statistics-table-robustness-simulated-p-value"
  ) %>%
  add_header_above(c(" " = 2, "KS Statistic (Robustness Check)" = 4)) %>%
  write_lines("tables/ks-statistics-table-robustness-simulated-p-value.tex")


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
