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
  group_by(source, nudge_type) %>%
  summarize(
    total_points.mean = mean(total_points),
    total_points.sd = sd(total_points),
    total_points.lci = total_points.mean - 1.96 * total_points.sd / sqrt(n()),
    total_points.uci = total_points.mean + 1.96 * total_points.sd / sqrt(n())
  )

random_payoff = 150
maximum_payoff = 183.63861

plot.optimal_earnings <- d.summary %>%
  ggplot(aes(nudge_type, total_points.mean, color = source)) +
  geom_point(position = position_dodge(0.4)) +
  geom_line(aes(group = source), size = 1, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = total_points.lci, ymax = total_points.uci), width = 0.2, position = position_dodge(0.4)) +
  geom_hline(yintercept = random_payoff, linetype = "dashed", color = "darkgreen") +
  geom_hline(yintercept = maximum_payoff, linetype = "dashed", color = "darkgreen") +
  scale_y_continuous(expand = c(0, 4)) +
  scale_color_d3() +
  facet_wrap(~ source, ncol = 4) +
  theme_minimal() +
  xlab("Nudge Type") +
  ylab("Total Points") +
  theme(legend.position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    legend.position = "none"
  )

plot.optimal_earnings %>% ggsave(
  "figures/plot-optimal_earnings.png",
  width = 8,
  height = 4,
  dpi = 300,
  plot = .
)

plot.reveals <- d %>%
  ggplot(aes(n_uncovered, color = source, fill = source)) +
  geom_dots(binwidth = 0.8, overflow = TRUE) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = scales::percent) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  facet_grid(nudge_type ~ source) +
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
  "figures/plot-optimal_reveals.png",
  width = 10,
  height = 2.8,
  plot = .
)


plot.ks <- d %>%
  filter(source != "Human") %>%
  group_by(source, nudge_type) %>%
  summarize(
    ks_stat = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human", nudge_type == nudge_type) %>%
        pull(n_uncovered)
    )$statistic,
    ks_p = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human", nudge_type == nudge_type) %>%
        pull(n_uncovered)
    )$p.value
  ) %>%
  arrange(ks_stat) %>%
  as.data.frame() %>%
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  ) %>%
  ggplot(aes(source, ks_stat, fill = nudge_type)) +
  geom_col(position = position_dodge2(0.8, preserve = "single"), width = 0.8) +
  geom_text(
    aes(label = ifelse(ks_p < 0.0001, "****", ifelse(ks_p < 0.001, "***", ifelse(ks_p < 0.01, "**", ifelse(ks_p < 0.05, "*", ""))))),
    position = position_dodge2(0.8, preserve = "single"),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.05))) +
  scale_fill_npg() +
  xlab("Source") +
  ylab("KS Statistic") +
  guides(fill = guide_legend(title = "Nudge Type")) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

plot.ks %>% ggsave(
  "figures/plot-optimal_ks.png",
  width = 10,
  height = 2,
  plot = .
)


emm.earnings <- d %>%
  lmer(
    total_points ~ source * nudge_type + (1 | participant_id),
    data = .
  ) %>%
  emmeans(
    ~ source | nudge_type
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
  file = "tables/optimal-earnings-emmeans.txt",
  sep = "\n",
  append = FALSE
)

contrast.earnings %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/optimal-earnings-contrast.txt",
  sep = "\n",
  append = FALSE
)

est.ks <- d %>%
  filter(source != "Human") %>%
  group_by(source, method) %>%
  summarize(
    ks_stat = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human") %>%
        pull(n_uncovered)
    )$statistic,
    ks_p = ks.test(
      n_uncovered,
      d %>%
        filter(source == "Human") %>%
        pull(n_uncovered)
    )$p.value
  ) %>%
  arrange(ks_stat) %>%
  as.data.frame() %>%
  mutate(
    ks_p = p.adjust(ks_p, method = "BH")
  )

est.ks %>% knitr::kable(digits = 4) %>% cat(
  file = "tables/optimal-ks-test.txt",
  sep = "\n",
  append = FALSE
)
