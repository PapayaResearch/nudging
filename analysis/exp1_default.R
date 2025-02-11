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
  is_nudge_index_optimal = nudge_index == optimal_option,
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

plot.nudge <- d.summary %>% ggplot(aes(trial_nudge, nudge_prob, color = Features, linetype = Options)) +
  geom_point() +
  geom_line(aes(group = interaction(Features, Options)), size = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.5, 1), labels=scales::percent) +
  scale_color_manual(values = c("gray", "red")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  facet_grid(method ~ source) +
  xlab("Nudge") +
  ylab("P(Choose Nudge)") +
  guides(color = guide_legend(title = "Prizes")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


plot.nudge %>% ggsave(
  "figures/plot-nudge.png",
  width = 12,
  height = 4,
  plot = .
)

plot.default <- d %>%
  group_by(source, method, trial_nudge, Features, Options, is_nudge_index_optimal) %>%
  summarize(
    accepted_default = mean(
      accepted_default %>% recode(
        "True" = 1,
        "False" = 0
      )
    )
  ) %>%
  mutate(
    is_nudge_index_optimal = ifelse(is_nudge_index_optimal, "Opt.", "Subopt.")
  ) %>%
  subset(trial_nudge == "Present") %>%
  ggplot(aes(is_nudge_index_optimal, accepted_default, fill = interaction(Features, Options, sep = "x"))) +
  geom_col(width = 0.6, position = position_dodge(0.8)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.5, 1), labels=scales::percent) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_fill_manual(values = c(
    "gray50",
    "gray25",
    "red",
    "darkred"
  )) +
  facet_grid(method ~ source) +
  xlab("Nudge Optimality") +
  ylab("P(Accept Default)") +
  labs(fill = "Grid Shape") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


plot.default %>% ggsave(
  "figures/plot-default.png",
  width = 12,
  height = 4,
  plot = .
)


random_payoff = 150
maximum_payoff = 183.63861

plot.idiosyncracy <- d %>% ggplot(aes(idiosyncracy, total_points, color = trial_nudge, fill = trial_nudge)) +
  geom_smooth(alpha = 0.2) +
  stat_summary_bin(fun.data=mean_se, bins=5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_colour_manual(values=c(
    "gray75",
    "dodgerblue"
  ), aesthetics=c("fill", "colour"), name="Nudge") +
  geom_hline(yintercept=c(random_payoff), linetype="dotted") +
  geom_hline(yintercept=c(maximum_payoff), linetype="dotted", color="darkred") +
  facet_grid(method ~ source) +
  xlab("Idiosyncracy") +
  ylab("Total Points") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.idiosyncracy %>% ggsave(
  "figures/plot-idiosyncracy.png",
  width = 12,
  height = 4,
  plot = .
)


plot.reveals <- d %>%
  subset(trial_nudge == "Absent") %>%
  ggplot(aes(n_uncovered, color = source, fill = source)) +
  geom_dots(binwidth = 0.8, overflow = TRUE) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25)) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = scales::percent) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  facet_grid(method ~ source) +
  xlab("Number of Cells Uncovered") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

plot.reveals %>% ggsave(
  "figures/plot-reveals.png",
  width = 10,
  height = 2.8,
  plot = .
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
    "GPT-3.5 Turbo / Human" = c(-1, 1, 0, 0, 0, 0, 0, 0),
    "GPT-4o Mini / Human" = c(-1, 0, 1, 0, 0, 0, 0, 0),
    "GPT-4o / Human" = c(-1, 0, 0, 1, 0, 0, 0, 0),
    "Gemini 1.5 Flash / Human" = c(-1, 0, 0, 0, 1, 0, 0, 0),
    "Gemini 1.5 Pro / Human" = c(-1, 0, 0, 0, 0, 1, 0, 0),
    "Claude 3.5 Haiku / Human" = c(-1, 0, 0, 0, 0, 0, 1, 0),
    "Claude 3.5 Sonnet / Human" = c(-1, 0, 0, 0, 0, 0, 0, 1)
  ),
  adjust = "BH"
)

d %>%
  subset(trial_nudge == "Present") %>% mutate(
  accepted_default = chose_nudge %>% recode("True" = 1, "False" = 0)
) %>% group_by(
  source,
  method
) %>% summarize(
  nudge_prob = mean(accepted_default)
) %>% arrange(nudge_prob)


plot.ks <- d %>%
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
  as.data.frame() %>%
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
  guides(fill = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.ks %>% ggsave(
  "figures/plot-ks.png",
  width = 10,
  height = 2,
  plot = .
)

emm_options(lmerTest.limit = 6400, pbkrtest.limit = 3000)

emm.earnings <- plot.earnings <- d %>%
  lmer(
    total_points ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .
  ) %>%
  emmeans(
    ~ source | trial_nudge
  )

plot.earnings <- emm.earnings %>%
  as_tibble() %>%
  ggplot(aes(source, emmean, fill = trial_nudge)) +
  geom_col(
    width = 0.8,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.8),
    width = 0.1
  ) +
  geom_hline(yintercept = random_payoff, linetype = "dashed", color = "darkgreen") +
  geom_hline(yintercept = maximum_payoff, linetype = "dashed", color = "darkgreen") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.05))) +
  scale_fill_manual(values = c("gray40", "darkred")) +
  xlab("") +
  ylab("Points Earned") +
  guides(fill = guide_legend(title = "Nudge")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.earnings %>% ggsave(
  "figures/plot-default_earnings.png",
  width = 10,
  height = 2.4,
  plot = .
)


emm.earnings <- d %>%
  lmer(
    total_points ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .
  ) %>%
  emmeans(
    ~ source | trial_nudge
  )

contrast.earnings <- emm.earnings %>% contrast(
  method = list(
    "GPT-3.5 Turbo / Human" = c(-1, 1, 0, 0, 0, 0, 0, 0),
    "GPT-4o Mini / Human" = c(-1, 0, 1, 0, 0, 0, 0, 0),
    "GPT-4o / Human" = c(-1, 0, 0, 1, 0, 0, 0, 0),
    "Gemini 1.5 Flash / Human" = c(-1, 0, 0, 0, 1, 0, 0, 0),
    "Gemini 1.5 Pro / Human" = c(-1, 0, 0, 0, 0, 1, 0, 0),
    "Claude 3.5 Haiku / Human" = c(-1, 0, 0, 0, 0, 0, 1, 0),
    "Claude 3.5 Sonnet / Human" = c(-1, 0, 0, 0, 0, 0, 0, 1)
  ),
  adjust = "BH"  
)

emm.earnings %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/default-earnings-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.earnings %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/default-earnings-contrast.txt",
  sep = "\n",
  append = FALSE
)


emm.nudge <- d %>%
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
  ) %>% emmeans(
    ~ source | trial_nudge,
    type = "response"
  )

contrast.nudge <- emm.nudge %>% contrast(
  method = list(
    "GPT-3.5 Turbo / Human" = c(-1, 1, 0, 0, 0, 0, 0, 0),
    "GPT-4o Mini / Human" = c(-1, 0, 1, 0, 0, 0, 0, 0),
    "GPT-4o / Human" = c(-1, 0, 0, 1, 0, 0, 0, 0),
    "Gemini 1.5 Flash / Human" = c(-1, 0, 0, 0, 1, 0, 0, 0),
    "Gemini 1.5 Pro / Human" = c(-1, 0, 0, 0, 0, 1, 0, 0),
    "Claude 3.5 Haiku / Human" = c(-1, 0, 0, 0, 0, 0, 1, 0),
    "Claude 3.5 Sonnet / Human" = c(-1, 0, 0, 0, 0, 0, 0, 1)
  ),
  adjust = "BH"  ,
  type = "response"
)

emm.nudge %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/default-nudge-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.nudge %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/default-nudge-contrast.txt",
  sep = "\n",
  append = FALSE
)


est.ks <- d %>%
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
  as.data.frame() %>%
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  )

est.ks %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/default-ks-test.txt",
  sep = "\n",
  append = FALSE
)
