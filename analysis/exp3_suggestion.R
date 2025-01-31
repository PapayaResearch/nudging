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

plot.suggestion_nudge <- d %>%
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0.5, 1), labels = scales::percent) +
  scale_color_uchicago() +
  xlab("Nudge") +
  ylab("P(Choose Nudge)") +
  guides(color = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.suggestion_nudge %>% ggsave(
  "figures/plot-suggestion_nudge.png",
  width = 10,
  height = 2.4,
  plot = .
)

table.change_selection <- d %>%
  subset(trial_nudge == "Late" & source != "Human") %>%
  mutate(
    changed_selection = first_selected_option != selected_option
  ) %>% group_by(
    source, method, should_change
  ) %>% summarize(
    changed_selection_prob = mean(changed_selection) * 100
  ) %>% arrange(should_change, changed_selection_prob) %>%
  knitr::kable(
    format = "latex",
    caption = "Probability of changing selection after nudge",
    booktabs = TRUE,
    col.names = c("Model", "Method", "Should Change", "P(Change)"),
    digits = 2
  )

table.change_selection %>% cat(
  file = "tables/table-change_selection.tex"
)

plot.change_optimality <- d %>%
  subset(trial_nudge == "Late" & source != "Human") %>%
  mutate(
    changed_selection = first_selected_option != selected_option
  ) %>% group_by(
    source,
    method,
    should_change
  ) %>% summarize(
    changed_selection_prob = mean(changed_selection)
  ) %>% arrange(should_change, changed_selection_prob) %>%
  ggplot(aes(should_change %>% ifelse(., "Opt.", "Subopt."), changed_selection_prob, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  facet_grid(~ source) +
  scale_fill_uchicago() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1), breaks = c(0.5, 1), labels = scales::percent) +
  xlab("Change Optimality") +
  ylab("P(Change)") +
  guides(fill = guide_legend(title = "Method")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


plot.change_optimality %>% ggsave(
  "figures/plot-change_optimality.png",
  width = 10,
  height = 2.4,
  plot = .
)


table.nudge_difference <- d %>%
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
    nudge_prob_diff = diff(nudge_prob) * 100
  ) %>% arrange(nudge_prob_diff) %>%
  knitr::kable(
    format = "latex",
    caption = "Difference in P(Nudge) between optimal and suboptimal nudge",
    booktabs = TRUE,
    col.names = c("Model", "Method", "P(Opt.) - P(Subopt.)"),
    digits = 2
  )

table.nudge_difference %>% cat(
  file = "tables/table-nudge_difference.tex"
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
  scale_fill_manual(values = c("gray40", "darkred", "darkblue")) +
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
    "figures/plot-suggestion_earnings.png",
    width = 10,
    height = 2,
    plot = .
  )


plot.reveals <- d %>%
  subset(trial_nudge == "Absent") %>%
  ggplot(aes(n_uncovered, color = source, fill = source)) +
  geom_dots(binwidth = 0.8, overflow = TRUE) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
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
  "figures/plot-suggestion_reveals.png",
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
  "figures/plot-suggestion_ks.png",
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
  file = "tables/suggestion-earnings-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.earnings %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/suggestion-earnings-contrast.txt",
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
  subset(
    trial_nudge != "Absent"
  ) %>% glmer(
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
  file = "tables/suggestion-nudge-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.nudge %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/suggestion-nudge-contrast.txt",
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
  file = "tables/suggestion-ks-test.txt",
  sep = "\n",
  append = FALSE
)
