library(tidyverse)

d <- read.csv("data/default_data.csv")
d <- d %>% mutate(
  n_prizes = factor(actual_baskets),
  n_baskets = factor(num_features),
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
  Options = n_baskets
)
d <- d %>% subset(!is_practice)

d.summary <- d %>%
  group_by(trial_nudge, Features, Options) %>%
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
  xlab("Nudge") +
  ylab("Prob Choose Nudge") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d.summary %>% subset(trial_nudge == "Present") %>%
  ggplot(aes(Options, accepted_default)) +
  geom_col(position = "dodge") +
  facet_grid(
    ~ Features,
    labeller = labeller(
      Features = function(x) paste("Features=", x, sep = "")
    )
  ) +
  xlab("") +
  ylab("Prob Accept Default") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


random_payoff = 150
maximum_payoff = 183.63861

d %>% ggplot(aes(weights_deviation, total_points, color = trial_nudge, fill = trial_nudge)) +
  geom_smooth(alpha = 0.2) +
  stat_summary_bin(fun.data=mean_se, bins=5) +
  scale_colour_manual(values=c(
    "gray75",
    "dodgerblue"
  ), aesthetics=c("fill", "colour"), name="Nudge") +
  geom_hline(yintercept=c(maximum_payoff), linetype="dashed") +
  coord_cartesian(xlim=c(NULL), ylim=c(random_payoff, maximum_payoff)) +
  xlim(0, 40) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
