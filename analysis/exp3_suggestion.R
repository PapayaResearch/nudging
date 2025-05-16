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

# ============================================================================
# DATA PREPROCESSING
# ============================================================================

data_raw <- read.csv("data/data-suggestion.csv")
d <- preprocess_data(data_raw, nudge_type = "suggestion")
message(paste("Processed", nrow(d), "trials from suggestion nudge experiment"))

# ============================================================================
# PLOT FOR SUGGESTION EFFECT
# ============================================================================

plot_suggestion_nudge <- d %>%
  filter(trial_nudge != "Abs.") %>%
  group_by(source, source_parent, source_child, method, trial_nudge) %>%
  summarize(
    nudge_acceptance = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    .groups = "drop"
  ) %>%
  arrange(nudge_acceptance) %>%
  ggplot(aes(trial_nudge, nudge_acceptance, color = method)) +
  geom_point(size = 1.5) +
  geom_line(aes(group = method), size = 0.6) +
  ggh4x::facet_nested( ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0.25, 0.5, 0.75, 1),
    labels = scales::percent
  ) +
  scale_color_prompt() +
  labs(
    x = "Suggestion Timing",
    y = "P(Choose Suggested)"
  ) +
  guides(color = guide_legend(title = "Method")) +
  theme_nudge()

# Save suggestion effect plot
plot_suggestion_nudge %>% ggsave(
  "figures/plot-suggestion_nudge.png",
  width = 8,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# TABLE FOR CHANGE ANALYSIS
# ============================================================================

# Create table showing probability of changing selection after late suggestion
table_change_selection <- d %>%
  filter(trial_nudge == "Late" & source != "Human") %>%
  mutate(
    changed_selection = first_selected_option != selected_option
  ) %>%
  group_by(source, method, should_change) %>%
  summarize(
    changed_selection_prob = mean(changed_selection) * 100,
    .groups = "drop"
  ) %>%
  arrange(should_change, changed_selection_prob) %>%
  knitr::kable(
    format = "latex",
    caption = "Probability of Changing Selection After Suggestion (%)",
    booktabs = TRUE,
    col.names = c("Model", "Method", "Should Change", "P(Change)"),
    digits = 2
  )

# Save change selection table
table_change_selection %>% cat(
  file = "tables/table-change_selection.tex"
)

# ============================================================================
# PLOT FOR SELECTION CHANGE OPTIMALITY ANALYSIS
# ============================================================================

plot_change_optimality <- d %>%
  filter(trial_nudge == "Late" & source != "Human") %>%
  mutate(
    changed_selection = first_selected_option != selected_option
  ) %>%
  group_by(source, source_parent, source_child, method, should_change) %>%
  summarize(
    changed_selection_prob = mean(changed_selection),
    .groups = "drop"
  ) %>%
  arrange(should_change, changed_selection_prob) %>%
  ggplot(aes(
    should_change %>% ifelse(., "Opt.", "Sub."),
    changed_selection_prob,
    fill = method
  )) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  ggh4x::facet_nested( ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  scale_fill_prompt() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, 1),
    breaks = c(0.25, 0.5, 0.75, 1),
    labels = scales::percent
  ) +
  labs(
    x = "Change Optimality",
    y = "P(Change Selection)"
  ) +
  guides(fill = guide_legend(title = "Method")) +
  theme_nudge()

# Save change optimality plot
plot_change_optimality %>% ggsave(
  "figures/plot-change_optimality.png",
  width = 10.4,
  height = 2,
  dpi = 300,
  plot = .
)

# ============================================================================
# TABLE FOR DIFFERENCE ANALYSIS
# ============================================================================

# Create table showing difference in nudge acceptance between optimal and suboptimal nudges
table_nudge_difference <- d %>%
  filter(trial_nudge == "Early") %>%
  # Create nudge optimality var
  mutate(
    nudge_optimal = nudge_index == optimal_option
  ) %>%
  group_by(source, method, nudge_optimal) %>%
  summarize(
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    .groups = "drop"
  ) %>%
  arrange(nudge_optimal, nudge_prob) %>%
  group_by(source, method) %>%
  summarize(
    nudge_prob_diff = diff(nudge_prob) * 100,
    .groups = "drop"
  ) %>%
  # Sort by the difference
  arrange(nudge_prob_diff) %>%
  knitr::kable(
    format = "latex",
    caption = "Difference in Nudge Acceptance between Optimal and Suboptimal Suggestions",
    booktabs = TRUE,
    col.names = c("Model", "Method", "P(Accept Optimal) - P(Accept Suboptimal)"),
    digits = 2
  )

# Save nudge difference table
table_nudge_difference %>% cat(
  file = "tables/table-nudge_difference.tex"
)

# ============================================================================
# EARNINGS ANALYSIS
# ============================================================================

emm_earnings <- create_earnings_model(d)

# Create custom earnings plot for suggestion experiment (this one has 3 conditions)
plot_earnings <- emm_earnings %>%
  as_tibble() %>%
  ggplot(aes(source, emmean, fill = trial_nudge)) +
  geom_col(
    width = 0.8,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.8),
    width = 0.1
  ) +
  geom_hline(
    yintercept = RANDOM_PAYOFF,
    linetype = "dashed",
    color = "darkgreen",
    alpha = 0.7,
    size = 0.8
  ) +
  geom_hline(
    yintercept = MAXIMUM_PAYOFF,
    linetype = "dashed",
    color = "darkred",
    alpha = 0.7,
    size = 0.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.05))) +
  scale_fill_manual(values = c("gray40", "darkred", "darkblue")) +
  labs(
    x = "",
    y = "Points Earned"
  ) +
  guides(fill = guide_legend(title = "Timing")) +
  theme_nudge()

# Save custom earnings plot
plot_earnings %>% ggsave(
  "figures/plot-suggestion_earnings.png",
  width = 10.4,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR INFORMATION GATHERING
# ============================================================================

# Create reveals plot using standard function
plot_reveals <- plot_reveals(
  d,
  facet_formula = "method ~ source_parent + source_child",
  filter_condition = "trial_nudge == 'Abs.'"
)

# Save reveal counts plot
plot_reveals %>% ggsave(
  "figures/plot-suggestion_reveals.png",
  width = 8,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# KS ANALYSIS
# ============================================================================

# Calculate KS statistics to compare the distributions with human data
ks_stats <- calculate_ks_stats(d, filter_condition = "trial_nudge == 'Abs.'")
plot_ks <- plot_ks(ks_stats)

# Save KS statistic plot
plot_ks %>% ggsave(
  "figures/plot-suggestion_ks.png",
  width = 10,
  height = 2.8,
  dpi = 300,
  plot = .
)

# ============================================================================
# OTHER MODELING
# ============================================================================

contrast_earnings <- create_human_contrasts(emm_earnings)
emm_nudge <- d %>%
  # Convert chosen nudge to binary
  mutate(
    chose_nudge = chose_nudge %>% recode(
      "True" = 1,
      "False" = 0
    )
  ) %>%
  filter(trial_nudge != "Abs.") %>%
  glmer(
    chose_nudge ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .,
    family = binomial
  ) %>%
  emmeans(
    ~ source | trial_nudge,
    type = "response"
  )
contrast_nudge <- create_human_contrasts(emm_nudge)

# ============================================================================
# STORE RESULTS
# ============================================================================

save_table(emm_earnings, "tables/suggestion-earnings-emmeans.txt")
save_table(contrast_earnings, "tables/suggestion-earnings-contrast.txt")

save_table(emm_nudge, "tables/suggestion-nudge-emmeans.txt")
save_table(contrast_nudge, "tables/suggestion-nudge-contrast.txt")

save_table(ks_stats, "tables/suggestion-ks-test.txt")
