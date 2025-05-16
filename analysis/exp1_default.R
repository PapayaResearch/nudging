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

data_raw <- read.csv("data/data-default.csv")
d <- preprocess_data(data_raw, nudge_type = "default")
message(paste("Processed", nrow(d), "trials from default nudge experiment"))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

d_summary <- d %>%
  group_by(source, source_parent, source_child, method, trial_nudge, Features, Options) %>%
  summarize(
    # Calculate proportion of choosing the nudge
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    # Calculate proportion of accepting the default
    default_acceptance = mean(
      accepted_default %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    .groups = "drop"
  )

# ============================================================================
# PLOT FOR NUDGE EFFECT
# ============================================================================

# Create plot showing probability of choosing default option
plot_nudge <- d_summary %>%
  ggplot(aes(trial_nudge, nudge_prob, color = Features, linetype = Options)) +
  geom_point(size = 1) +
  geom_line(aes(group = interaction(Features, Options)), size = 0.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.5, 1), labels = scales::percent) +
  scale_color_manual(values = c("gray40", "firebrick1")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  ggh4x::facet_nested(method ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  labs(
    x = "Nudge Condition",
    y = "P(Choose Default)"
  ) +
  guides(color = guide_legend(title = "Prizes")) +
  theme_nudge()

# Save nudge effect plot
plot_nudge %>% ggsave(
  "figures/plot-nudge.png",
  width = 8,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR DEFAULT OPTIMALITY
# ============================================================================

# Create plot showing default acceptance rates by optimality
plot_default <- d %>%
  group_by(source, source_parent, source_child, method, trial_nudge, Features, Options, is_nudge_index_optimal) %>%
  summarize(
    default_acceptance = mean(
      accepted_default %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    .groups = "drop"
  ) %>%
  # Label optimality
  mutate(
    nudge_optimality = ifelse(is_nudge_index_optimal, "Opt.", "Sub.")
  ) %>%
  # Filter to only trials with the nudge Pres.
  filter(trial_nudge == "Pres.") %>%

  ggplot(aes(nudge_optimality, default_acceptance,
             fill = interaction(Features, Options, sep = "x"))) +
  geom_col(width = 0.6, position = position_dodge(0.8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.5, 1), labels = scales::percent) +
  scale_fill_manual(values = c(
    "gray50",
    "gray25",
    "firebrick3",
    "firebrick4"
  )) +
  ggh4x::facet_nested(method ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  labs(
    x = "Default Optimality",
    y = "P(Accept Default)",
    fill = "Grid Shape"
  ) +
  theme_nudge()

# Save default optimality plot
plot_default %>% ggsave(
  "figures/plot-default.png",
  width = 8,
  height = 3.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR IDIOSYNCRASY, EARNINGS
# ============================================================================

# Create plot showing relationship between idiosyncrasy and earnings
plot_idiosyncrasy <- d %>%
  ggplot(aes(idiosyncracy, total_points, color = trial_nudge, fill = trial_nudge)) +
  geom_smooth(alpha = 0.2, method = "loess", lwd = 0.4) +
  stat_summary_bin(fun.data = mean_se, bins = 5, size = 0.1) +
  scale_x_continuous(expand = c(0, 0), breaks = c(10, 20, 30)) +
  scale_colour_manual(
    values = c("gray50", "dodgerblue4"),
    aesthetics = c("fill", "colour"),
    name = "Nudge"
  ) +
  # Add reference lines for baseline performance
  geom_hline(yintercept = RANDOM_PAYOFF, linetype = "dotted", color = "darkgreen", size = 0.4) +
  geom_hline(yintercept = MAXIMUM_PAYOFF, linetype = "dotted", color = "darkred", size = 0.4) +
  ggh4x::facet_nested(method ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  labs(
    x = "Idiosyncrasy Score",
    y = "Total Points"
  ) +
  theme_nudge()

# Save idiosyncrasy earnings plot
plot_idiosyncrasy %>% ggsave(
  "figures/plot-idiosyncracy.png",
  width = 8,
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
  "figures/plot-reveals.png",
  width = 8,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# STATISTICAL MODELING
# ============================================================================

# Analyze default acceptance using mixed-effects model
default_acceptance_model <- d %>%
  mutate(
    accepted_default = accepted_default %>% recode("True" = 1, "False" = 0)
  ) %>%
  glmer(
    accepted_default ~ source + (1 | method) + (1 | participant_id),
    family = binomial,
    data = .
  )

# Calculate contrasts with human acceptance rates
acceptance_contrasts <- default_acceptance_model %>%
  emmeans(~ source) %>%
  create_human_contrasts()

# Calculate nudge probability for each model and method
nudge_summary <- d %>%
  filter(trial_nudge == "Pres.") %>%
  mutate(
    accepted_default = chose_nudge %>% recode("True" = 1, "False" = 0)
  ) %>%
  group_by(source, source_parent, source_child, method) %>%
  summarize(
    nudge_prob = mean(accepted_default),
    .groups = "drop"
  ) %>%
  arrange(nudge_prob)

print(nudge_summary)

# ============================================================================
# KS ANALYSIS
# ============================================================================

# Calculate KS statistics to compare agent distributions with human data
ks_stats <- calculate_ks_stats(d, filter_condition = "trial_nudge == 'Abs.'")
plot_ks <- plot_ks(ks_stats, fill_var = "method", x_var = "source")

# Save KS statistic plot
plot_ks %>% ggsave(
  "figures/plot-ks.png",
  width = 10,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# EARNINGS ANALYSIS
# ============================================================================

# Create earnings model, plot
emm_earnings <- create_earnings_model(d)
plot_earnings <- plot_earnings(
  emm_earnings
)

# Save the earnings plot
plot_earnings %>% ggsave(
  "figures/plot-default_earnings.png",
  width = 10.4,
  height = 2.4,
  dpi = 300,
  plot = .
)

# Create contrasts comparing each model to human performance
contrast_earnings <- create_human_contrasts(emm_earnings)

# ============================================================================
# STORE RESULTS
# ============================================================================

save_table(emm_earnings, "tables/default-earnings-emmeans.txt")
save_table(contrast_earnings, "tables/default-earnings-contrast.txt")

# Create nudge model and contrasts
emm_nudge <- d %>%
  mutate(
    chose_nudge = chose_nudge %>% recode(
      "True" = 1,
      "False" = 0
    )
  ) %>%
  glmer(
    chose_nudge ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .,
    family = binomial
  ) %>%
  emmeans(
    ~ source | trial_nudge,
    type = "response"
  )

# Create contrasts for nudge acceptance
contrast_nudge <- create_human_contrasts(emm_nudge)

# Save nudge tables
save_table(emm_nudge, "tables/default-nudge-emmeans.txt")
save_table(contrast_nudge, "tables/default-nudge-contrast.txt")

# Save KS test results
save_table(ks_stats, "tables/default-ks-test.txt")
