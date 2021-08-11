set.seed(5585) # random.org: Min: 1, Max: 100000

library(tidyverse)
options(dplyr.summarise.inform = FALSE)

theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

focus_states <- c("MI", "MO")
mult <- seq(0, 0.5, length.out = 50)
num_sims <- 100

all_res <- list()

for (state in focus_states) {

  message(paste0(state, "...."))

  fname <- paste0(state, ".csv")
  demos <- read_csv(
    paste0("../data/vf_demographic/", fname),
    progress = FALSE, col_types = cols())

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
  orig_age_dict <- age_dict

  for (j in 1:length(mult)) {
    
    message(paste0(j, ".."))

    for (sim in 1:num_sims) {

      age_dict <- orig_age_dict
      foo <- qnorm(age_dict$pred_rate)
      age_dict$pred_rate <- pnorm(foo + mult[j] * rnorm(length(foo)))

      county_dict <- d %>%
      group_by(county, age) %>% 
      summarize(
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

      county_res <- county_dict %>% 
        group_by(county) %>% 
        summarize(
          r_count = cor(pred_count, true_count),
          r_rate = cor(pred_rate, true_rate),
          n_registered = sum(bin_size)
        ) %>% 
        ungroup()

      county_res$state <- state
      county_res$mult <- mult[j]
      county_res$mult_idx <- j
      county_res$sim_idx <- sim

      all_res <- append(all_res, list(county_res))

      
    }

  }

}

df <- all_res %>% bind_rows()
df %>% write_csv(
  "../results/noise_simulation_results.csv"
)

df <- read_csv("../results/noise_simulation_results.csv")


pdf <- df %>%
  pivot_longer(
    contains("r_"),
    names_prefix = "r_",
    names_to = "input",
    values_to = "r"
  ) %>% 
  group_by(input, state, mult_idx, sim_idx) %>% 
  summarize(r = mean(r), mult = mult[1]) %>% 
  ungroup() %>% 
  group_by(input, state, mult_idx) %>% 
  summarize(
    m = mean(r), s = sd(r),
    mult = mult[1]
  ) %>% 
  ungroup() %>% 
  mutate(
    lwr = m - 2 * s,
    upr = m + 2 * s
  )

pdf %>% 
  mutate(input = case_when(
    input == "rate" ~ "Turnout Rate",
    input == "count" ~ "Ballot Count",
  )) %>%
  rename(Statistic = input) %>%  
  ggplot(aes(x = mult, y = m, ymin = lwr,
    ymax = upr)) +
  geom_line(aes(color = Statistic)) +
  geom_ribbon(aes(fill = Statistic), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_grey() +
  scale_fill_grey() +
  facet_wrap(~state) +
  theme(legend.position="top") +
  labs(x = "Noise Factor", y = "Correlation")
ggsave("../results/noise_paths.pdf", width = 10, height = 5)

