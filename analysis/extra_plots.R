library(tidyverse)
library(lme4)
library(emmeans)
library(ggdist)
library(ggsci)

d <- read.csv("data/data-default.csv")
d <- d %>% mutate(
  n_prizes = factor(n_prizes),
  n_baskets = factor(n_baskets),
  trial_nudge = trial_nudge %>% recode(
    "control" = "Absent",
    "default" = "Present"
  ),
  is_practice = is_practice %>% recode(
    "True" = TRUE,
    "False" = FALSE
  ),
  total_points = net_earnings * 3000,
  Features = n_prizes,
  Options = n_baskets,
  source = source %>% recode(
    "real" = "Human",
    "gpt-3.5-turbo-0125" = "GPT-3.5 Turbo",
    "gpt-4o-mini-2024-07-18" = "GPT-4o Mini",
    "gemini/gemini-1.5-flash" = "Gemini 1.5 Flash",
    "gemini/gemini-1.5-pro" = "Gemini 1.5 Pro",
    "claude-3-5-sonnet-20241022" = "Claude 3.5 Sonnet",
    "claude-3-haiku-20240307" = "Claude 3 Haiku",
    "gpt-4o-2024-08-06" = "GPT-4o"
  ) %>% factor(levels = c(
    "Human",
    "GPT-3.5 Turbo",
    "GPT-4o Mini",
    "GPT-4o",
    "Gemini 1.5 Flash",
    "Gemini 1.5 Pro",
    "Claude 3 Haiku",
    "Claude 3.5 Sonnet"
  )),
  method = ifelse(cot, "CoT", ifelse(fs > 0, "FS", "Base"))
)
d <- d %>% subset(!is_practice)

d$uncovered_values <- d$uncovered_values %>%
  str_replace_all("\\[|\\]", "") %>%
  str_split(", ") %>%
  map(~ as.integer(.x))


d <- d %>%
  mutate(
    n_options_num = as.integer(as.character(n_prizes)),
    n_baskets_num = as.integer(as.character(n_baskets))
  )

SUBSET <- "Absent"
d <- d %>% subset(trial_nudge == SUBSET)

d.exp <- d %>%
  unnest_longer(
    col = uncovered_values,
    indices_to = "step",
    values_to  = "uncovered_value"
  ) %>% drop_na(
    uncovered_value
  ) %>% mutate(
    row = (uncovered_value %/% n_baskets_num) + 1,
    col = (uncovered_value %% n_baskets_num) + 1
  )

d.salience <- d.exp %>%
  group_by(source, method, n_baskets_num, n_options_num) %>%
  mutate(N = n()) %>%
  group_by(source, method, n_baskets_num, n_options_num, row, col) %>%
  summarize(
    n_reveals = n(),
    prop_reveals = n_reveals / unique(N),
    .groups = "drop"
  )

for (m in c("Base", "FS", "CoT")) {
  plot.salience <- d.salience %>%
    subset(
      method == m | source == "Human"
    ) %>%
    mutate(
      grid = interaction(n_options_num, n_baskets_num, sep = "x")
    ) %>%
    group_by(source, row, col, grid) %>%
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
    ggplot(aes(x = col, y = row, fill = n_reveals)) +
    geom_tile() +
    ggh4x::facet_grid2(
      grid ~ source,
      scales = "free",
      independent = "all"
    ) +
    scale_y_reverse() +
    scale_fill_viridis_c() +
    guides(fill = guide_colorbar(title = "Reveals")) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  plot.salience %>% ggsave(
    paste("figures/plot-salience_", m, ".png", sep = ""),
    width = ifelse(m == "FS", 9.2, 12),
    height = 4,
    plot = .
  )
  
}
