library(tidyverse)
library(lme4)
library(emmeans)
library(ggdist)
library(ggsci)

d <- read.csv("data/data-optimal.csv")
d <- d %>% mutate(
  n_prizes = factor(n_prizes),
  n_baskets = factor(n_baskets),
  nudge_type = nudge_type %>% recode(
    "random" = "Random",
    "extreme" = "Extreme",
    "greedy" = "Optimal"
  ) %>% factor(levels = c("Random", "Extreme", "Optimal")),
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
  method = ifelse(cot, "CoT", ifelse(fs > 0, "FS", "Base"))
)
d <- d %>% subset(!is_practice)

d.summary <- d %>%
  group_by(source, nudge_type) %>%
  summarize(
    total_points.mean = mean(total_points),
    total_points.sd = sd(total_points),
    total_points.lci = total_points.mean - 1.96 * total_points.sd / sqrt(n()),
    total_points.uci = total_points.mean + 1.96 * total_points.sd / sqrt(n())
  )

d.summary %>%
  ggplot(aes(nudge_type, total_points.mean, color = source)) +
  geom_point(position = position_dodge(0.4)) +
  geom_line(aes(group = source), size = 1, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = total_points.lci, ymax = total_points.uci), width = 0.2, position = position_dodge(0.4)) +
  scale_y_continuous(expand = c(0, 4)) +
  scale_color_d3() +
  facet_wrap(~ source, scales = "free_y") +
  theme_minimal() +
  xlab("Nudge Type") +
  ylab("Total Points") +
  theme(legend.position = "bottom") +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black")
  )
