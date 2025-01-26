library(tidyverse)
library(lme4)
library(emmeans)
library(ggdist)
library(ggsci)

d <- read.csv("data/data-suggestion.csv")
d <- d %>% mutate(
  n_prizes = factor(n_prizes),
  n_baskets = factor(n_baskets),
  trial_nudge = trial_nudge %>% recode(
    "control" = "Absent",
    "pre-supersize" = "Early",
    "post-supersize" = "Late"
  ) %>% factor(levels = c("Absent", "Early", "Late")),
  is_practice = is_practice %>% recode(
    "True" = TRUE,
    "False" = FALSE
  ),
  should_change = value_first_option_selected < value_final_option_selected,
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
  subset(trial_nudge != "Absent") %>%
  group_by(source, method, trial_nudge, Features) %>%
  summarize(
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    )
  )

d %>%
  subset(trial_nudge != "Absent") %>%
  group_by(source, method, trial_nudge) %>%
  summarize(
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    )
  ) %>% arrange(nudge_prob) %>%
  ggplot(aes(trial_nudge, nudge_prob, color = method)) +
  geom_point() +
  geom_line(aes(group = method), size = 1) +
  facet_grid(~ source) +
  scale_color_aaas() +
  xlab("Nudge") +
  ylab("P(Nudge)") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>%
  subset(trial_nudge == "Late" & source != "Human") %>%
  mutate(
    changed_selection = first_selected_option != selected_option
  ) %>% group_by(
    source, method, should_change
  ) %>% summarize(
    changed_selection_prob = mean(changed_selection)
  ) %>% arrange(should_change, changed_selection_prob) %>% print(n = 100)

d %>%
  subset(trial_nudge == "Early") %>%
  mutate(
    nudge_optimal = nudge_index == optimal_option
  ) %>% group_by(
    source, method, nudge_optimal
  ) %>% summarize(
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    )
  ) %>% arrange(
    nudge_optimal,
    nudge_prob
  )%>% group_by(source, method) %>%
  summarize(
    nudge_prob_diff = diff(nudge_prob)
  ) %>% arrange(nudge_prob_diff) %>% print(n = 100)
