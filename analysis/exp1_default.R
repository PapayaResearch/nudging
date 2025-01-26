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
    "gpt-4o-2024-08-06" = "GPT-4o"
  ) %>% factor(levels = c("Human", "GPT-3.5 Turbo", "GPT-4o Mini", "Gemini 1.5 Flash", "GPT-4o")),
  method = ifelse(cot, "CoT", ifelse(fs > 0, "FS", "Base"))
)
d <- d %>% subset(!is_practice)

d.summary <- d %>%
  group_by(source, method, trial_nudge, Features, Options) %>%
  summarize(
    nudge_prob = mean(
      chose_nudge %>% recode(
        "True" = 1,
        "False" = 0
      )
    ),
    accepted_default = mean(
      accepted_default %>% recode(
        "True" = 1,
        "False" = 0
      )
    )
  )

d.summary %>% ggplot(aes(trial_nudge, nudge_prob, color = Features, linetype = Options)) +
  geom_point() +
  geom_line(aes(group = interaction(Features, Options)), size = 1) +
  scale_color_manual(values = c("gray", "red")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  facet_grid(method ~ source) +
  xlab("Nudge") +
  ylab("P(Default)") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


random_payoff = 150
maximum_payoff = 183.63861

d %>% ggplot(aes(idiosyncracy, total_points, color = trial_nudge, fill = trial_nudge)) +
  geom_smooth(alpha = 0.2) +
  stat_summary_bin(fun.data=mean_se, bins=5) +
  scale_colour_manual(values=c(
    "gray75",
    "dodgerblue"
  ), aesthetics=c("fill", "colour"), name="Nudge") +
  geom_hline(yintercept=c(random_payoff), linetype="dotted") +
  geom_hline(yintercept=c(maximum_payoff), linetype="dotted", color="darkred") +
  facet_grid(method ~ source) +
  xlim(0, 40) +
  ylim(120, 200) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>%
  subset(trial_nudge == "Absent") %>%
  ggplot(aes(n_uncovered, color = source, fill = source)) +
  geom_dots(binwidth = 0.8, overflow = TRUE) +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  facet_grid(method ~ source) +
  xlab("Number of Cells Uncovered") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>% mutate(
  accepted_default = accepted_default %>% recode("True" = 1, "False" = 0)
) %>% glmer(
  accepted_default ~ source + (1 | method) + (1 | participant_id),
  family = binomial,
  data = .
) %>% emmeans(
  ~ source
) %>% contrast(
  method = list(
    "GPT-3.5 Turbo / Human" = c(-1, 1, 0, 0, 0),
    "GPT-4o Mini / Human" = c(-1, 0, 1, 0, 0),
    "Gemini 1.5 Flash / Human" = c(-1, 0, 0, 1, 0),
    "GPT-4o / Human" = c(-1, 0, 0, 0, 1)
  ),
  adjust = "BH"
)


d %>%
  filter(trial_nudge == "Absent", source != "Human") %>%
  group_by(source, method) %>%
  summarize(
    ks_stat = ks.test(
      n_uncovered,
      d %>%
        filter(trial_nudge == "Absent", source == "Human") %>%
        pull(n_uncovered)
    )$statistic,
    ks_p = ks.test(
      n_uncovered,
      d %>%
        filter(trial_nudge == "Absent", source == "Human") %>%
        pull(n_uncovered)
    )$p.value
  ) %>%
  arrange(ks_stat) %>%
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  ) %>%
  ggplot(aes(source, ks_stat, fill = method)) +
  geom_col(position = position_dodge2(0.8, preserve = "single"), width = 0.8) +
  geom_text(
    aes(label = ifelse(ks_p < 0.0001, "****", ifelse(ks_p < 0.001, "***", ifelse(ks_p < 0.01, "**", ifelse(ks_p < 0.05, "*", ""))))),
    position = position_dodge2(0.8, preserve = "single"),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.05))) +
  scale_fill_uchicago() +
  xlab("Source") +
  ylab("KS Statistic") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
