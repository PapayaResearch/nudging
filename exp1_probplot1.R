library(tidyverse)

d <- read.csv("data.csv")
d <- d %>% mutate(
  n_prizes = factor(n_prizes),
  n_baskets = factor(n_baskets),
  source = source %>% recode(
    "gpt-4o-mini" = "GPT-4o-Mini +Nudge",
    "gpt-4o-mini-vanilla" = "GPT-4o-Mini",
    "gpt-4o-mini-reason" = "GPT-4o-Mini +Reason",
    "gpt-4o-mini-reason-posthoc" = "GPT-4o-Mini +Post-Reason",
    "gpt-4o-mini-nudge-pd" = "GPT-4o-Mini +PD",
    "gpt-3.5-turbo-vanilla" = "GPT-3.5-Turbo",
    "real" = "Real"
  ),
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

d <- d %>%
  group_by(participant_id, trial_num) %>%
  mutate(
    weights_deviation = ifelse(source == "Real", weights_deviation, weights_deviation[source == "Real"])
  ) %>%
  ungroup()

d.summary <- d %>%
  group_by(source, trial_nudge, Features, Options) %>%
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
  facet_wrap(~source) +
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
  ggplot(aes(Options, accepted_default, color = source, fill = source)) +
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

random_payoff = 20
maximum_payoff = 300

d %>% ggplot(aes(weights_deviation, total_points, color = trial_nudge, fill = trial_nudge)) +
  geom_smooth(alpha = 0.2) +
  stat_summary_bin(fun.data=mean_se, bins=5) +
  facet_wrap(~ source) +
  scale_colour_manual(values=c(
    "gray75",
    "dodgerblue"
  ), aesthetics=c("fill", "colour"), name="Nudge") +
  geom_hline(yintercept=c(maximum_payoff), linetype="dashed") +
  coord_cartesian(xlim=c(NULL), ylim=c(random_payoff, maximum_payoff)) +
  xlim(0, 32) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>% ggplot(aes(trial_nudge, gross_earnings * 3000, color = source, fill = source)) +
  geom_boxplot() +
  xlab("Nudge") +
  ylab("Gross Points") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

d %>% ggplot(aes(trial_nudge, total_points, color = source, fill = source)) +
  geom_boxplot() +
  xlab("Nudge") +
  ylab("Total Points") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

d <- d %>% mutate(
  uncovered_values = uncovered_values %>% gsub("(\\[|\\])", "", .) %>% str_split(", ") %>% sapply(factor),
  n_uncovered = uncovered_values %>% sapply(length)
)

d %>% ggplot(aes(trial_nudge, n_uncovered, color = source, fill = source)) +
  geom_boxplot() +
  xlab("Nudge") +
  ylab("N Uncovered") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )


d %>%
  group_by(Features, Options, trial_nudge, source) %>%
  summarize(
    n_uncovered.mean = mean(n_uncovered),
    n_uncovered.sd = sd(n_uncovered),
    n_uncovered.se = n_uncovered.sd / sqrt(n()),
    n_uncovered.ci = qt(0.975, n() - 1) * n_uncovered.se
  ) %>%
  ggplot(aes(trial_nudge, n_uncovered.mean, color = source, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.6) +
  geom_errorbar(aes(ymin = n_uncovered.mean - n_uncovered.ci, ymax = n_uncovered.mean + n_uncovered.ci), width = 0.1, position = position_dodge(0.8), color = "black") +
  facet_grid(
    Features ~ Options,
    labeller = labeller(
      Features = function(x) paste("Features=", x, sep = ""),
      Options = function(x) paste("Options=", x, sep = "")
    )
  ) +
  xlab("Nudge") +
  ylab("N Uncovered") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

d %>%
  group_by(trial_nudge, participant_id, trial_num) %>%
  mutate(
    accepted_agreement = accepted_default == accepted_default[source == "Real"]
  ) %>%
  group_by(source) %>%
  subset(trial_nudge == "Present") %>%
  subset(source != "Real") %>%
  summarize(
    n_accepted_agreement = mean(accepted_agreement)
  )


string_to_matrix <- function(s) {
  # Remove outer brackets and split into rows
  row_strings <- strsplit(gsub("^\\[\\[|\\]\\]$", "", s), "\\],\\s*\\[")[[1]]
  
  # Split each row into individual elements and count columns
  row_list <- lapply(row_strings, function(row) as.numeric(unlist(strsplit(row, ","))))
  
  # Determine the number of columns (assuming each row has the same number of columns)
  ncol <- length(row_list[[1]])
  
  # Convert to matrix
  matrix(unlist(row_list), ncol = ncol, byrow = TRUE)
}

string_to_vector <- function(s) {
  s <- gsub("\\[|\\]", "", s)  # Remove square brackets
  as.numeric(unlist(strsplit(s, ",")))  # Split by comma and convert to numeric
}

d <- d %>%
  mutate(
    payoff_matrix = payoff_matrix %>% lapply(string_to_matrix),
    weights = weights %>% lapply(string_to_vector),
    basket_values = mapply(function(m, v) t(m) %*% v, payoff_matrix, weights),
    optimal_basket = sapply(basket_values, which.max) - 1,
    selected_optimal = selected_option == optimal_basket
  )

d %>% group_by(source, trial_nudge, Options, Features) %>%
  summarize(optimal = mean(selected_optimal)) %>%
  ggplot(aes(trial_nudge, optimal, color = source, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.6) +
  facet_grid(
    Features ~ Options,
    labeller = labeller(
      Features = function(x) paste("Features=", x, sep = ""),
      Options = function(x) paste("Options=", x, sep = "")
    )
  ) +
  xlab("Nudge") +
  ylab("Optimal Selected") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

d <- d %>% mutate(
  uncovered_values_numeric = lapply(uncovered_values, function(x) as.numeric(as.character(x))),
  uncovered_rows = Map(function(values, n) floor(values / n) + 1, uncovered_values_numeric, as.numeric(as.character(n_baskets))),
  uncovered_cols = Map(function(values, n) (values %% n) + 1, uncovered_values_numeric, as.numeric(as.character(n_baskets))),
  uncovered_rowweights = Map(function(rows, cols, weights) weights[cbind(rows, cols)], uncovered_rows, uncovered_cols, weights),
  uncovered_rowweightsums = uncovered_rowweights %>% lapply(sum) %>% as.numeric(),
  uncovered_rowweightmeans = uncovered_rowweightsums / n_uncovered
)

d %>%
  subset(!is.na(uncovered_rowweightmeans)) %>%
  ggplot((aes(trial_nudge, uncovered_rowweightmeans, color = source, fill = source))) +
  geom_boxplot() +
  xlab("Nudge") +
  ylab("Uncovered Row Weight Averages") +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(color = "black"),
    axis.line.x.bottom = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
