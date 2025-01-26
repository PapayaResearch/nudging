library(tidyverse)
library(lme4)
library(emmeans)
library(ggdist)
library(ggsci)

d <- read.csv("data/data-highlight.csv")
d <- d %>% mutate(
  n_prizes = factor(n_prizes),
  n_baskets = factor(n_baskets),
  trial_nudge = is_control %>% recode(
    "True" = "Absent",
    "False" = "Present"
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
    "gpt-4o-2024-08-06" = "GPT-4o"
  ) %>% factor(levels = c("Human", "GPT-3.5 Turbo", "GPT-4o Mini", "Gemini 1.5 Flash", "GPT-4o")),
  highlight_revealsn = highlight_reveals / (n_uncovered + 1e-8),
  weights = weights %>% str_extract_all("\\d+(\\.\\d+)?") %>% map(as.numeric),
  highest_prize = (weights %>% map_dbl(which.max)) - 1,
  is_nudge_index_optimal = nudge_index == highest_prize,
  is_first_index_nudged = is_first_index_nudged %>% recode(
    "True" = 1,
    "False" = 0
  ),
  method = ifelse(cot, "CoT", ifelse(fs > 0, "FS", "Base"))
)
d <- d %>% subset(!is_practice)

d %>%
  subset(trial_nudge == "Present") %>%
  group_by(is_nudge_index_optimal, source, method, trial_nudge) %>%
  summarize(
    highlight_revealsn.mean = mean(highlight_revealsn),
    highlight_revealsn.sd = sd(highlight_revealsn),
    highlight_revealsn.lci = highlight_revealsn.mean - 1.96 * highlight_revealsn.sd / sqrt(n()),
    highlight_revealsn.uci = highlight_revealsn.mean + 1.96 * highlight_revealsn.sd / sqrt(n()),
    highlight_value = mean(highlight_value),
    n_uncovered = mean(n_uncovered)
  ) %>% arrange(is_nudge_index_optimal, highlight_revealsn.mean) %>% print(n = 100)

d %>%
  subset(trial_nudge == "Present") %>%
  group_by(source, method, trial_nudge, is_nudge_index_optimal) %>%
  summarize(
    highlight_revealsn.mean = mean(highlight_revealsn),
    highlight_revealsn.sd = sd(highlight_revealsn),
    highlight_revealsn.lci = highlight_revealsn.mean - 1.96 * highlight_revealsn.sd / sqrt(n()),
    highlight_revealsn.uci = highlight_revealsn.mean + 1.96 * highlight_revealsn.sd / sqrt(n()),
    highlight_value = mean(highlight_value),
    n_uncovered = mean(n_uncovered)
  ) %>% arrange(highlight_revealsn.mean) %>%
  ggplot(aes(highlight_value, highlight_revealsn.mean, color = source, linetype = method, shape = method)) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = highlight_revealsn.lci, ymax = highlight_revealsn.uci), width = 0) +
  scale_color_uchicago() +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  facet_wrap(~ is_nudge_index_optimal) +
  xlab("Highlight Value") +
  ylab("Highlight Reveals") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

d %>%
  group_by(source, method, trial_nudge) %>%
  ggplot(aes(highlight_value, highlight_revealsn, color = source, fill = source, linetype = method, shape = method)) +
  geom_smooth(alpha = 0.1) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Highlight Value") +
  ylab("Highlight Reveals") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>%
  subset(trial_nudge == "Present") %>%
  group_by(source, method, trial_nudge, is_nudge_index_optimal) %>%
  summarize(is_first_index_nudged = mean(is_first_index_nudged)) %>%
  arrange(is_nudge_index_optimal, is_first_index_nudged) %>% print(n=100)
