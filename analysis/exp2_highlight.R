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

data_raw <- read.csv("data/data-highlight.csv")
d <- preprocess_data(data_raw, nudge_type = "highlight")
message(paste("Processed", nrow(d), "trials from highlight nudge experiment"))

# ============================================================================
# PLOT FOR HIGHLIGHT OPTIMALITY
# ============================================================================

# Create plot showing relationship between highlight value and reveal proportion grouped by highlight optimality
plot_highlight_optimal <- d %>%
  filter(trial_nudge == "Pres.") %>%
  group_by(source, source_parent, source_child, method, trial_nudge, is_nudge_index_optimal) %>%
  summarize(
    # Calculate highlight reveal statistics
    highlight_revealsn_mean = mean(highlight_revealsn),
    highlight_revealsn_sd = sd(highlight_revealsn),
    highlight_revealsn_lci = highlight_revealsn_mean - 1.96 * highlight_revealsn_sd / sqrt(n()),
    highlight_revealsn_uci = highlight_revealsn_mean + 1.96 * highlight_revealsn_sd / sqrt(n()),
    highlight_value = mean(highlight_value),
    n_uncovered = mean(n_uncovered),
    .groups = "drop"
  ) %>%
  # Sort by reveal proportion for a cleaner plot
  arrange(highlight_revealsn_mean) %>%
  ggplot(aes(highlight_value, highlight_revealsn_mean, color = source, shape = method)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = source), linetype = "dashed", alpha = 0.3) +
  geom_errorbar(aes(ymin = highlight_revealsn_lci, ymax = highlight_revealsn_uci),
                width = 0, alpha = 0.6) +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_color_npg() +
  facet_wrap(~ is_nudge_index_optimal %>% ifelse(., "Optimal Highlighting", "Suboptimal Highlighting"), scales = "fixed") +
  labs(
    x = "Highlight Value",
    y = "Highlighted Cells Revealed (%)"
  ) +
  guides(
    color = guide_legend(title = "Source"),
    shape = guide_legend(title = "Method")
  ) +
  theme_nudge()

# Save highlight optimality plot
plot_highlight_optimal %>% ggsave(
  "figures/plot-highlight_optimal.png",
  width = 10,
  height = 4,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR HIGHLIGHT VALUE
# ============================================================================

# Create plot showing relationship between highlight value and reveal proportion across different models, methods
plot_highlight_revealsvalues <- d %>%
  ggplot(aes(highlight_value, highlight_revealsn, color = method, fill = method)) +
  geom_smooth(method = "loess", alpha = 0.2) +
  scale_color_prompt() +
  scale_fill_prompt() +
  scale_x_continuous(expand = c(0, 0), breaks = c(5, 10), labels = c("5", "10")) +
  scale_y_continuous(expand = c(0, 0.1), breaks = c(0, 0.5, 1), labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~ source, ncol = 3) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs(
    x = "Highlight Value",
    y = "% Reveals from Highlighted"
  ) +
  guides(
    color = guide_legend(title = "Method"),
    fill = guide_legend(title = "Method")
  ) +
  theme_nudge()

# Save highlight values plot
plot_highlight_revealsvalues %>% ggsave(
  "figures/plot-highlight_revealsvalues.png",
  width = 8,
  height = 3.2,
  dpi = 300,
  plot = .
)

# ============================================================================
# PLOT FOR FIRST REVEALS
# ============================================================================

# Create plot showing probability of first reveal from nudged prize by optimality
plot_highlight_optimality <- d %>%
  filter(trial_nudge == "Pres.") %>%
  group_by(source, source_parent, source_child, method, trial_nudge, is_nudge_index_optimal) %>%
  summarize(
    first_reveal_nudged = mean(is_first_index_nudged),
    .groups = "drop"
  ) %>%
  arrange(is_nudge_index_optimal, first_reveal_nudged) %>%
  ggplot(aes(
    is_nudge_index_optimal %>% ifelse(., "Opt.", "Sub."),
    first_reveal_nudged,
    color = method
  )) +
  geom_point(size = 1.5) +
  geom_line(aes(group = method), size = 0.6) +
  scale_color_prompt() +
  facet_wrap(~ source, ncol = 3) +
  scale_fill_prompt() +
  scale_y_continuous(
    expand = c(0, 0.1),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = scales::percent
  ) +
  labs(
    x = "Highlight Optimality",
    y = "P(First Reveal from Highlighted)"
  ) +
  guides(color = guide_legend(title = "Method")) +
  theme_nudge()

# Save highlight optimality plot
plot_highlight_optimality %>% ggsave(
  "figures/plot-highlight_optimality.png",
  width = 6,
  height = 3.2,
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

# Save reveals plot
plot_reveals %>% ggsave(
  "figures/plot-highlight_reveals.png",
  width = 8,
  height = 2.4,
  dpi = 300,
  plot = .
)

# ============================================================================
# KS ANALYSIS
# ============================================================================

ks_stats <- calculate_ks_stats(d, filter_condition = "trial_nudge == 'Abs.'")
plot_ks <- plot_ks(ks_stats)

# Save KS statistic plot
plot_ks %>% ggsave(
  "figures/plot-highlight_ks.png",
  width = 10,
  height = 2.8,
  dpi = 300,
  plot = .
)

# ============================================================================
# EARNINGS ANALYSIS
# ============================================================================

# Create earnings model and visualization
emm_earnings <- create_earnings_model(d)
plot_earnings <- plot_earnings(
  emm_earnings
)

# Save earnings plot
plot_earnings %>% ggsave(
  "figures/plot-highlight_earnings.png",
  width = 10.4,
  height = 2.4,
  dpi = 300,
  plot = .
)

 # Create contrasts comparing each model to human performance
contrast_earnings <- create_human_contrasts(emm_earnings)

# ============================================================================
# REVEALS ANALYSIS
# ============================================================================

emm_nudge <- d %>%
  lmer(
    highlight_revealsn ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .
  ) %>% emmeans(
    ~ source | trial_nudge
  )

contrast_nudge <- create_human_contrasts(emm_nudge)

# ============================================================================
# STORE RESULTS
# ============================================================================

save_table(emm_earnings, "tables/highlight-earnings-emmeans.txt")
save_table(contrast_earnings, "tables/highlight-earnings-contrast.txt")

save_table(emm_nudge, "tables/highlight-nudge-emmeans.txt")
save_table(contrast_nudge, "tables/highlight-nudge-contrast.txt")

save_table(ks_stats, "tables/highlight-ks-test.txt")
