library(tidyverse)
library(ggrepel)
library(cowplot)

theme_set(theme_cowplot())

states <- list.files("../data/vf_demographic/") %>%
  str_replace(".csv", "") %>%
  sort()

for (state in states) {
  message(paste0(state, "..."))

  fname <- paste0(state, ".csv")
  demos <- read_csv(
    paste0("../data/vf_demographic/", fname),
    progress = FALSE, col_types = cols())

  miss_rate <- mean(is.na(demos$Voters_Age))
  if (miss_rate > 0.1) {
    message(paste0("SKIPPING ", state, " (missing ages)"))
    next()
  }

  history <- read_csv(
    paste0("../data/vf_history/", fname),
    progress = FALSE, col_types = cols()) %>%
    select(-State)

  demos <- demos %>%
    left_join(history, by = "LALVOTERID")

  rm(list = c("history"))

  d <- demos
  rm(list = c("demos"))

  d <- d %>%
    select(-LALVOTERID, -State) %>% 
    rename(
      age = Voters_Age,
      turnout = General_2020_11_03,
      county = County
    )

  d <- d %>% filter(!is.na(age))

  d <- d %>%
    mutate(turnout = case_when(
      turnout == "Y" ~ 1,
      TRUE ~ 0
    ))

  ages <- d %>% pull(age) %>% unique() %>% sort()
  fit <- d %>% lm(turnout ~ poly(age, 6), data = .)
  age_dict <- expand.grid(age = ages) %>% tibble()
  age_dict$pred_rate <- predict(fit, newdata = age_dict)

  county_dict <- d %>%
    group_by(county, age) %>% 
    summarize(
      age = age[1],
      bin_size = n(),
      true_rate = mean(turnout),
      true_count = sum(turnout)
    ) %>% 
    ungroup()

  county_dict <- county_dict %>% 
    left_join(age_dict, by = "age")

  county_dict <- county_dict %>% 
    mutate(
      pred_count = pred_rate * bin_size
    )

  county_dict %>% write_csv(paste0(
    "../results/county_predictions/",
    state,
    ".csv"
  ))

  county_res <- county_dict %>% 
    group_by(county) %>% 
    summarize(
      max_age = max(age, na.rm = TRUE),
      r_count = cor(pred_count, true_count),
      r_rate = cor(pred_rate, true_rate),
      n_registered = sum(bin_size)
    ) %>% 
    ungroup()

  county_res$state <- state

  county_res %>%
    write_csv(paste0(
      "../results/county_correlations/",
      state,
      ".csv"
    ))
}


usable_states <- list.files("../results/county_correlations/") %>%
  str_replace(".csv", "") %>%
  sort()



# County Variance Analysis ------------------------------------------------

d <- list()
for (i in 1:length(usable_states)) {
  d[[i]] <- read_csv(paste0(
    "../results/county_predictions/",
    usable_states[i],
    ".csv"
  ),
  progress = FALSE,
  col_types = cols()
  ) %>% 
    mutate(state = usable_states[i])
}

d <- bind_rows(d)


d %>% 
  filter(state %in% c("OH", "PA", "AZ", "GA")) %>% 
  ggplot(aes(age, true_rate)) +
  facet_wrap(~state) +
  geom_point(shape = 1, alpha = 0.5) +
  labs(y = "Turnout Rate", x = "Age (Years)",
       title = "Variation in County Turnout Rates by Age")
ggsave("../results/turnout_spread_select.pdf")

d %>% 
  ggplot(aes(age, true_rate)) +
  facet_wrap(~state) +
  geom_point(shape = 1, alpha = 0.1) +
  labs(y = "Turnout Rate", x = "Age (Years)",
       title = "Variation in County Turnout Rates by Age")
# ggsave("../results/turnout_spread_all.pdf") # actually have to manually save this in Rstudio because it's too large for ggplot on my device



sds <- d %>%
  group_by(state, age) %>% 
  summarize(s = sd(true_rate)) %>% 
  ungroup()
  
sds %>% 
  ggplot(aes(age, s)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Age (Years)",
       y = "Std. Deviation of County Turnout",
       title = "Within-State, Within-Age Std. Deviations of County Turnout"m
       subtitle = "(Points correspond to states")
ggsave("../results/turnout_sd.pdf")


# Correlations Analysis ---------------------------------------------------


d <- list()
for (i in 1:length(usable_states)) {
  d[[i]] <- read_csv(paste0(
      "../results/county_correlations/",
      usable_states[i],
      ".csv"
    ),
    progress = FALSE,
    col_types = cols()
    )
}

d <- bind_rows(d)

sdf <- d %>%
  group_by(state) %>% 
  summarize(r_count = mean(r_count), r_rate = mean(r_rate))

xymin <- sdf %>% select(r_count, r_rate) %>% summarize_all(min) %>% min()


elections <- read_csv("../data/1976-2020-president.csv")
# from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
elections <- elections %>%
  filter(year == 2020) %>% 
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  group_by(state_po) %>% 
  summarize(winner = party_simplified[which.max(candidatevotes)]) %>% 
  rename(state = state_po)

sdf <- sdf %>% 
  left_join(elections, by = "state")

party_colors <- c("#0015BC", "#FF0000")

options(ggrepel.max.overlaps = Inf)

base <- sdf %>%
  ggplot(aes(r_rate, r_count, fill = winner, color = winner)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  coord_cartesian(xlim = c(xymin, 1), ylim = c(xymin, 1), clip = "off") +
  geom_text_repel(aes(label = state), size = 2.0) +
  labs(x = "Avg. Turnout Rate Correlation", y = "Avg. Ballot Count Correlation") +
  theme(legend.position = "None")

base +
  scale_color_manual(values = rep("#000000", 2))
ggsave("../results/state_r_nocolor.pdf", width = 6, height = 3)

base +
  scale_color_manual(values = party_colors)
ggsave("../results/state_r_yescolor.pdf", width = 6, height = 3)

# > sdf[which.min(abs(sdf$r_rate - mr)), ]
# # A tibble: 1 x 3
#   state r_count r_rate
#   <chr>   <dbl>  <dbl>
# 1 MO      0.992  0.769

# These states are unavailable because they have > 10% missing age
states[which(!states %in% usable_states)]







d <- list()
for (i in 1:length(usable_states)) {
  d[[i]] <- read_csv(paste0(
      "../results/county_predictions/",
      usable_states[i],
      ".csv"
    ),
    progress = FALSE,
    col_types = cols()
    )
  d[[i]]$state <- usable_states[i]
}

d <- bind_rows(d)

d %>%
  group_by(county) %>%
  summarize(
    r_rate = cor(pred_rate, true_rate),
    r_count = cor(pred_count, true_count)
  ) %>% 
  pivot_longer(contains("r_"), names_prefix = "r_", names_to = "input", values_to = "r") %>% 
  mutate(input = case_when(input == "rate" ~ "Turnout Rate", input == "count" ~ "Ballot Count" )) %>% 
  ggplot(aes(r)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ input, scale = "free_y") +
  labs(x = "County Correlation between Predicted and Actual", y = "",
  title = "Histogram: Counties from Available States")
ggsave("../results/county_r_hist.pdf", width = 8, height = 4)





d %>%
  filter(state == "MI") %>% 
  group_by(age) %>% 
  summarize(pred_rate = pred_rate[1]) %>%
  ggplot(aes(age, pred_rate)) +
  geom_line() +
  labs(y = "Predicted Turnout Rate", x = "Age", title = "Replication: Michigan's \"Key\"") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))
ggsave("../results/key_mi.pdf", width = 6, height = 4)






