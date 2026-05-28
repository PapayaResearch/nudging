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
# SETUP
# ============================================================================

OPTIMIZATION_DATA_PATH <- "data/data-optimal.csv"

OPTIMIZATION_SOURCE_LEVELS <- c(
  "Human",
  "GPT-4o Mini",
  "GPT-4o",
  "Claude 3 Haiku",
  "Claude 4.5 Sonnet",
  "o3 Mini",
  "GPT-5",
  "GPT-5 Mini"
)

OPTIMIZATION_METHOD_LEVELS <- c(
  "Random",
  "Extreme",
  "Optimal-RR",
  "Optimal-AI",
  "Optimal-MLP",
  "Optimal-Residual"
)

OPTIMIZATION_METHOD_COLORS <- c(
  "Max" = "red",
  "Min" = "#BDBDBD",
  "Other" = "black"
)

compute_rr_contrasts <- function(data) {
  contrast_rows <- data %>%
    group_by(source) %>%
    group_split() %>%
    map_dfr(function(source_data) {
      source_data <- source_data %>%
        droplevels()

      rr_index <- which(levels(source_data$nudge_type) == "Optimal-RR")
      source_name <- source_data %>%
        distinct(source) %>%
        pull(source) %>%
        as.character()

      model_source <- feols(
        total_points ~ nudge_type,
        data = source_data,
        vcov = cluster ~ participant_id
      )

      emm_source <- emmeans(
        model_source,
        ~ nudge_type,
        data = source_data
      )

      contrast(
        emm_source,
        method = "trt.vs.ctrl",
        ref = rr_index,
        adjust = "dunnettx"
      ) %>%
        summary(infer = TRUE) %>%
        as_tibble() %>%
        mutate(
          source = source_name,
          nudge_type = contrast %>%
            str_replace_all("\\(|\\)", "") %>%
            str_replace(" - Optimal-RR", "")
        ) %>%
        transmute(
          source = source,
          nudge_type = nudge_type,
          rr_difference = estimate,
          rr_lower_cl = lower.CL,
          rr_upper_cl = upper.CL,
          rr_p_value = p.value
        )
    })

  bind_rows(
    contrast_rows,
    data %>%
      distinct(source) %>%
      mutate(
        source = as.character(source),
        nudge_type = "Optimal-RR",
        rr_difference = 0,
        rr_lower_cl = NA_real_,
        rr_upper_cl = NA_real_,
        rr_p_value = NA_real_
      )
  ) %>%
    mutate(
      source = factor(source, levels = OPTIMIZATION_SOURCE_LEVELS),
      nudge_type = factor(nudge_type, levels = OPTIMIZATION_METHOD_LEVELS)
    ) %>%
    arrange(source, nudge_type)
}

build_optimization_table <- function(data) {
  table_data <- data %>%
    add_significance_stars(p_col = "rr_p_value") %>%
    mutate(
      emm_label = case_when(
        sig_stars == "" ~ paste0(round(emmean, 1), " (", round(SE, 1), ")"),
        TRUE ~ paste0(round(emmean, 1), "$^{", sig_stars, "}$ (", round(SE, 1), ")")
      )
    ) %>%
    select(source, nudge_type, emm_label) %>%
    pivot_wider(
      names_from = nudge_type,
      values_from = emm_label
    ) %>%
    mutate(
      across(
        all_of(OPTIMIZATION_METHOD_LEVELS),
        ~ replace_na(.x, "--")
      )
    ) %>%
    select(source, all_of(OPTIMIZATION_METHOD_LEVELS)) %>%
    rename(Model = source)

  table_data
}

if (!file.exists(OPTIMIZATION_DATA_PATH)) {
  stop("Missing optimal analysis data at `data/data-optimal.csv`.")
}

# ============================================================================
# LOAD DATA
# ============================================================================

d <- read.csv(OPTIMIZATION_DATA_PATH) %>%
  preprocess_data(nudge_type = "optimal") %>%
  filter(source %in% OPTIMIZATION_SOURCE_LEVELS) %>%
  mutate(
    source = factor(as.character(source), levels = OPTIMIZATION_SOURCE_LEVELS),
    nudge_type = factor(as.character(nudge_type), levels = OPTIMIZATION_METHOD_LEVELS)
  ) %>%
  droplevels()

# ============================================================================
# MODELS
# ============================================================================

optimization_model <- feols(
  total_points ~ source * nudge_type,
  data = d,
  vcov = cluster ~ participant_id
)

optimization_emm <- get_marginal_effects(
  optimization_model,
  ~ nudge_type | source,
  data = d
) %>%
  summary(infer = TRUE) %>%
  as_tibble() %>%
  mutate(
    source = as.character(source),
    nudge_type = as.character(nudge_type)
  )

optimization_rr_contrasts <- compute_rr_contrasts(d) %>%
  mutate(
    source = as.character(source),
    nudge_type = as.character(nudge_type)
  )

optimization_emm <- optimization_emm %>%
  left_join(
    optimization_rr_contrasts,
    by = c("source", "nudge_type")
  ) %>%
  mutate(
    source = factor(source, levels = OPTIMIZATION_SOURCE_LEVELS),
    nudge_type = factor(nudge_type, levels = OPTIMIZATION_METHOD_LEVELS)
  ) %>%
  arrange(source, nudge_type)

optimization_plot_data <- optimization_emm %>%
  group_by(source) %>%
  mutate(
    highlight = case_when(
      min_rank(desc(emmean)) == 1 ~ "Max",
      min_rank(emmean) == 1 ~ "Min",
      TRUE ~ "Other"
    )
  ) %>%
  ungroup()

optimization_counts <- d %>%
  count(source, nudge_type, name = "n_trials")

# ============================================================================
# SAVE RESULTS
# ============================================================================

write_csv(optimization_emm, "results/optimization-emmeans.csv")
write_csv(optimization_rr_contrasts, "results/optimization-rr-contrasts.csv")
write_csv(optimization_counts, "results/optimization-counts.csv")

# ============================================================================
# FIGURES
# ============================================================================

p.optimization <- optimization_plot_data %>%
  ggplot(aes(
    x = emmean,
    y = fct_rev(nudge_type),
    color = highlight
  )) +
  geom_errorbar(
    aes(
      xmin = lower.CL,
      xmax = upper.CL
    ),
    width = 0.16,
    linewidth = 0.4,
    orientation = "y"
  ) +
  geom_point(size = 2.6) +
  facet_wrap(~ source, ncol = 4) +
  scale_y_discrete(
    limits = rev(OPTIMIZATION_METHOD_LEVELS),
    drop = FALSE
  ) +
  scale_color_manual(
    values = OPTIMIZATION_METHOD_COLORS,
    guide = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  xlab("Estimated Total Points") +
  ylab("Optimization Method") +
  theme_nudge() +
  theme(
    legend.position = "none"
  )

p.optimization %>%
  ggsave(
    filename = "figures/optimization-emmeans.pdf",
    plot = .,
    width = 11,
    height = 8
  )

# ============================================================================
# TABLES
# ============================================================================

optimization_model %>%
  make.regression_table(output.path = "tables/optimization-model_total_points.tex")

optimization_table <- build_optimization_table(optimization_emm)

optimization_table %>%
  kbl("markdown") %>%
  write_lines("tables/optimization-emmeans.md")

optimization_table %>%
  kbl(
    "latex",
    booktabs = TRUE,
    linesep = "",
    escape = FALSE,
    caption = paste(
      "Estimated marginal means (SE) for total points in the optimization experiment.",
      "Stars denote Dunnett-adjusted comparisons against Optimal-RR within source."
    ),
    position = "!htb",
    label = "optimization-emmeans"
  ) %>%
  write_lines("tables/optimization-emmeans.tex")
