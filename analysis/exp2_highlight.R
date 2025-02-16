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

plot.highlight_optimal <- d %>%
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
  ggplot(aes(highlight_value, highlight_revealsn.mean, color = source, shape = method)) +
  geom_point(size = 2) +
  geom_line(aes(group = source), linetype = "dashed", alpha = 0.2) +
  geom_errorbar(aes(ymin = highlight_revealsn.lci, ymax = highlight_revealsn.uci), width = 0, alpha = 0.6) +
  scale_y_continuous(breaks = c(0.5, 1), labels = scales::percent) +
  scale_color_npg() +
  facet_wrap(~ is_nudge_index_optimal %>% ifelse(., "Optimal", "Suboptimal")) +
  xlab("Highlight Value") +
  ylab("Highlight Reveals (as % of total)") +
  guides(shape = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.highlight_optimal %>% ggsave(
  "figures/plot-highlight_optimal.png",
  width = 8,
  height = 3.2,
  plot = .
)

plot.highlight_revealsvalues <- d %>%
  ggplot(aes(highlight_value, highlight_revealsn, color = method, fill = method)) +
  geom_smooth(alpha = 0.2) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  scale_x_continuous(expand = c(0, 0), breaks = c(5, 10), labels = c("5", "10")) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 0.5, 1), labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~ source, ncol = 4) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Highlight Value") +
  ylab("Highlight Reveals (as % of total)") +
  guides(color = guide_legend(title = "Method"), fill = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


plot.highlight_revealsvalues %>% ggsave(
  "figures/plot-highlight_revealsvalues.png",
  width = 8,
  height = 3.2,
  plot = .
)


plot.highlight_optimality <- d %>%
  subset(trial_nudge == "Present") %>%
  group_by(source, method, trial_nudge, is_nudge_index_optimal) %>%
  summarize(is_first_index_nudged = mean(is_first_index_nudged)) %>%
  arrange(is_nudge_index_optimal, is_first_index_nudged) %>%
  ggplot(aes(is_nudge_index_optimal %>% ifelse(., "Opt.", "Subopt."), is_first_index_nudged, color = method)) +
  geom_point() +
  geom_line(aes(group = method)) +
  scale_color_uchicago() +
  facet_wrap(~ source, ncol = 4) +
  scale_fill_uchicago() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), limits = c(0, 1), breaks = c(0.5, 1), labels = scales::percent) +
  xlab("Nudge Optimality") +
  ylab("P(1st Reveal from Nudged Prize)") +
  guides(color = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.highlight_optimality %>% ggsave(
  "figures/plot-highlight_optimality.png",
  width = 8,
  height = 2.4,
  plot = .
)

random_payoff = 150
maximum_payoff = 183.63861

emm_options(lmerTest.limit = 6400, pbkrtest.limit = 3000)

plot.earnings <- d %>%
  lmer(
    total_points ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .
  ) %>%
  emmeans(
    ~ source | trial_nudge
  ) %>%
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

plot.earnings %>%
  ggsave(
    "figures/plot-highlight_earnings.png",
    width = 10,
    height = 2.4,
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
  "figures/plot-highlight_reveals.png",
  width = 10,
  height = 2.8,
  plot = .
)


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
  "figures/plot-highlight_ks.png",
  width = 10,
  height = 2,
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
  file = "tables/highlight-earnings-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.earnings %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/highlight-earnings-contrast.txt",
  sep = "\n",
  append = FALSE
)


emm.nudge <- d %>%
  lmer(
    highlight_revealsn ~ source * trial_nudge + (1 | method) + (1 | participant_id),
    data = .
  ) %>% emmeans(
    ~ source | trial_nudge
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
  file = "tables/highlight-nudge-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.nudge %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/highlight-nudge-contrast.txt",
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
  file = "tables/highlight-ks-test.txt",
  sep = "\n",
  append = FALSE
)
