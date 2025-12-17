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
message(paste("Processed", nrow(d), "trials for extra visualizations"))

# Convert uncovered_values from string to list of integers for cell salience analysis
d$uncovered_values <- d$uncovered_values %>%
  str_replace_all("\\[|\\]", "") %>%
  str_split(", ") %>%
  map(~ as.integer(.x))

# Convert n_prizes and n_baskets to numeric for grid coordinate computation
d <- d %>%
  mutate(
    n_options_num = as.integer(as.character(n_prizes)),
    n_baskets_num = as.integer(as.character(n_baskets))
  )

# Filter to control for cleaner analysis
SUBSET <- "Abs."
d <- d %>% subset(trial_nudge == SUBSET)

# ============================================================================
# SALIENCE ANALYSIS
# ============================================================================

# Expand dataset to reveal-level data for salience analysis
d_expanded <- d %>%
  # Unnest lists of uncovered values into individual rows
  unnest_longer(
    col = uncovered_values,
    indices_to = "step",
    values_to = "uncovered_value"
  ) %>%
  drop_na(uncovered_value) %>%
  # Calculate row and column coordinates from uncovered value
  mutate(
    # 1D index -> 2D grid coords
    row = (uncovered_value %/% n_baskets_num) + 1,
    col = (uncovered_value %% n_baskets_num) + 1
  )

# Calculate salience metrics (frequency of cell reveals)
d_salience <- d_expanded %>%
  group_by(source, source_parent, source_child, method, n_baskets_num, n_options_num) %>%
  mutate(N = n()) %>%
  group_by(source, source_parent, source_child, method, n_baskets_num, n_options_num, row, col) %>%
  summarize(
    n_reveals = n(),
    prop_reveals = n_reveals / unique(N),
    .groups = "drop"
  )

salience_plots <- list()

# Create a salience plot for each method type
for (method_type in c("Base", "FS", "CoT")) {
  plot_salience <- d_salience %>%
    # Filter to current method and human for comparison
    subset(
      method == method_type | source == "Human"
    ) %>%
    mutate(
      grid = interaction(n_options_num, n_baskets_num, sep = "x")
    ) %>%
    group_by(source, source_parent, source_child, row, col, grid) %>%
    summarize(
      n_reveals = sum(n_reveals),
      prop_reveals = mean(prop_reveals),
      n_baskets_num = unique(n_baskets_num),
      n_options_num = unique(n_options_num),
      .groups = "drop"
    ) %>%
    group_by(grid) %>%
    mutate(source = droplevels(source)) %>%
    complete(
      source,
      col,
      row,
      fill = list(n_reveals = 0, prop_reveals = 0),
    ) %>%
    ungroup() %>%
    mutate( # nesting() in complete() causes some issues, so we need to re-do this
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
      )
    ) %>%
    ggplot(aes(x = col, y = row, fill = n_reveals)) +
    geom_tile() +
    ggh4x::facet_nested(
      grid ~ source_parent + source_child,
      scales = "free",
      independent = "all",
      nest_line = element_line(
        linewidth = 0.8,
        color = "black"
      )
    ) +
    scale_y_reverse() +
    scale_fill_viridis_c(
      limits = c(0, 60)
    ) +
    guides(fill = guide_colorbar(title = "Number of Reveals")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )

  # Save plot with method name in the filename
  plot_salience %>% ggsave(
    paste("figures/plot-salience_", method_type, ".pdf", sep = ""),
    width = ifelse(method_type == "FS", 12.2, 14),
    height = 4.5,
    dpi = 300,
    plot = .
  )
  
  salience_plots[[method_type]] <- plot_salience
}

# Combine salience plots into a single figure
p.salience_combined <- (salience_plots[["Base"]] + ggtitle("A. Base")) /
  (salience_plots[["CoT"]] + ggtitle("B. CoT")) /
  (salience_plots[["FS"]] + ggtitle("C. FS")) +
  plot_layout(
    guides = "collect",
    axes = "collect"
  ) &
  theme(
    legend.position = "bottom",
    panel.spacing.y = unit(0, "pt"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0)
  )

p.salience_combined

p.salience_combined %>%
  ggsave(
    "figures/plot-salience_combined.pdf",
    width = 10,
    height = 10,
    plot = .
  )
