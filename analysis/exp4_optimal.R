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

data_raw <- read.csv("data/data-optimal.csv")
d <- preprocess_data(data_raw, nudge_type = "optimal")
message(paste("Processed", nrow(d), "trials from optimal nudge experiment"))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

# Create summary for earnings analysis by nudge type
d_summary <- d %>%
  group_by(source, source_parent, source_child, nudge_type) %>%
  summarize(
    # Calculate the earnings statistics
    total_points_mean = mean(total_points),
    total_points_sd = sd(total_points),
    total_points_lci = total_points_mean - 1.96 * total_points_sd / sqrt(n()),
    total_points_uci = total_points_mean + 1.96 * total_points_sd / sqrt(n()),
    # Sample size per condition
    n_samples = n(),
    .groups = "drop"
  )

# ============================================================================
# PLOT FOR OPTIMAL EARNINGS
# ============================================================================

# Create plot showing earnings for each nudge type
plot_optimal_earnings <- d_summary %>%
  ggplot(aes(nudge_type, total_points_mean, color = source)) +
  # Point-interval plots
  geom_point(position = position_dodge(0.4), size = 1.5) +
  geom_line(aes(group = source), size = 0.6, position = position_dodge(0.4)) +
  geom_errorbar(
    aes(ymin = total_points_lci, ymax = total_points_uci),
    width = 0.2,
    position = position_dodge(0.4)
  ) +
  # Add reference lines for baseline performance
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
  scale_y_continuous(expand = c(0, 4)) +
  scale_color_d3() +
  # facet_wrap(~ source, ncol = 3) +
  ggh4x::facet_nested( ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  labs(
    x = "Nudge Type",
    y = "Total Points"
  ) +
  theme_nudge() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save optimal earnings plot
plot_optimal_earnings %>% ggsave(
  "figures/plot-optimal_earnings.png",
  width = 10,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR INFORMATION GATHERING
# ============================================================================

# Create reveals plot showing cells uncovered distribution by nudge type
plot_reveals <- d %>%
  ggplot(aes(n_uncovered, color = source, fill = source)) +
  geom_dots(binwidth = 0.8, overflow = TRUE) +
  scale_x_continuous(breaks = c(5, 15, 25)) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = scales::percent) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  ggh4x::facet_nested(nudge_type ~ source_parent + source_child, strip = ggh4x::strip_nested(bleed = TRUE)) +
  labs(
    x = "Number of Cells Uncovered",
    y = ""
  ) +
  theme_nudge() +
  theme(legend.position = "none")

# Save custom reveals plot
plot_reveals %>% ggsave(
  "figures/plot-optimal_reveals.png",
  width = 8.2,
  height = 2.8,
  dpi = 300,
  plot = .
)

# ============================================================================
# KS ANALYSIS
# ============================================================================

# Calculate KS statistics with nudge_type as grouping var
# Custom KS test for optimal experiment (considering nudge_type)
plot_ks <- d %>%
  filter(source != "Human") %>%
  group_by(source, source_parent, source_child, nudge_type) %>%
  summarize(
    # For each combination, test against human data with same nudge type
    ks_stat = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human", nudge_type == nudge_type) %>%
        pull(n_uncovered)
    )$statistic,
    ks_p = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human", nudge_type == nudge_type) %>%
        pull(n_uncovered)
    )$p.value,
    .groups = "drop"
  ) %>%
  arrange(ks_stat) %>%
  # Apply multiple comparison correction
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  ) %>%
  ggplot(aes(source, ks_stat, fill = nudge_type)) +
  geom_col(position = position_dodge2(0.8, preserve = "single"), width = 0.8) +
  geom_text(
    aes(
      label = ifelse(ks_p < 0.0001, "****",
        ifelse(ks_p < 0.001, "***",
          ifelse(ks_p < 0.01, "**",
            ifelse(ks_p < 0.05, "*", "")
          )
        )
      )
    ),
    position = position_dodge2(0.8, preserve = "single"),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.05))) +
  scale_fill_npg() +
  labs(
    caption = "* p<0.05, ** p<0.01, *** p<0.001, **** p<0.0001",
    x = "Source",
    y = "KS Statistic"
  ) +
  guides(fill = guide_legend(title = "Nudge Type")) +
  theme_nudge()

# Save KS statistic plot
plot_ks %>% ggsave(
  "figures/plot-optimal_ks.png",
  width = 10.4,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# TABLE FOR EARNINGS ANALYSIS
# ============================================================================

# Create earnings model with nudge_type instead of trial_nudge
emm_earnings <- d %>%
  lmer(
    total_points ~ source * nudge_type + (1 | participant_id),
    data = .
  ) %>%
  emmeans(
    ~ source | nudge_type
  )

# Create contrasts comparing each model to human performance
contrast_earnings <- create_human_contrasts(emm_earnings)

save_table(emm_earnings, "tables/optimal-earnings-emmeans.txt")
save_table(contrast_earnings, "tables/optimal-earnings-contrast.txt")

# ============================================================================
# OVERALL KS TEST ANALYSIS
# ============================================================================

# Calculate KS statistics for all data
ks_stats <- d %>%
  filter(source != "Human") %>%
  group_by(source, source_parent, source_child, nudge_type, method) %>%
  summarize(
    ks_stat = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human") %>%
        pull(n_uncovered)
    )$statistic,
    ks_p = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human") %>%
        pull(n_uncovered)
    )$p.value,
    .groups = "drop"
  ) %>%
  arrange(ks_stat) %>%
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  )

# Save KS test results
save_table(ks_stats, "tables/optimal-ks-test.txt")
