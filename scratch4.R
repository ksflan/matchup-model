





source("utils.R")



# Code for running OBP model

data_raw <- readRDS("data/retrosheet-10-17.rds")
event_values <- readRDS("retrosheet-event-values-1980-2017.rds") %>%
  mutate(event_cd = as.numeric(event_cd))

MINIMUM_ATBATS <- 600

# Different filtering options

sample_games <- sample(unique(data_raw$game_id), 20)
sample_batters <- data_raw %>%
  count(resp_bat_id) %>%
  filter(n > MINIMUM_ATBATS)
sample_pitchers <- data_raw %>%
  count(resp_pit_id) %>%
  filter(n > MINIMUM_ATBATS)

pitcher_atbat_counts <- data_raw %>%
  group_by(resp_pit_id) %>%
  summarise(pitcher_count = n())

batter_atbat_counts <- data_raw %>%
  group_by(resp_bat_id) %>%
  summarise(batter_count = n())

matchup_counts <- data_raw %>%
  count(resp_bat_id, resp_pit_id) %>%
  filter(n > 60)


# predata ----

pre_data <- data_raw %>%
  left_join(batter_atbat_counts) %>%
  left_join(pitcher_atbat_counts) %>%
  mutate(platoon = resp_bat_hand_cd == resp_pit_hand_cd,
         event_cd = as.numeric(event_cd),
         bat_home_id = as.numeric(bat_home_id),
         onbase = as.numeric(bat_dest_id) > 0) %>%
  filter(resp_bat_id %in% matchup_counts$resp_bat_id,
         resp_pit_id %in% matchup_counts$resp_pit_id,
         bat_event_fl == "true")


data <- list(
  N = nrow(pre_data),
  B = length(unique(pre_data$resp_bat_id)),
  P = length(unique(pre_data$resp_pit_id)),
  S = length(unique(pre_data$park_id)),
  batter = as.numeric(factor(pre_data$resp_bat_id)),
  pitcher = as.numeric(factor(pre_data$resp_pit_id)),
  stadium = as.numeric(factor(pre_data$park_id)),
  platoon = as.numeric(pre_data$resp_bat_hand_cd == pre_data$resp_pit_hand_cd),
  inning_bottom = pre_data$bat_home_id,
  onbase = pre_data$onbase
)

model3 <- stan(file = "stan/model-3.stan",
               data = data,
               iter = 1000,
               chains = 2)

summary <- rstan::summary(model3)$summary %>%
  as.data.frame()
summary$parameter <- rownames(summary)

beta <- summary %>%
  filter(grepl("beta", parameter),
         !grepl("star", parameter),
         !grepl("mu", parameter),
         !grepl("sigma", parameter)) %>%
  mutate(batter = as.character(levels(factor(pre_data$resp_bat_id))))



# Dynamic OBP model ----



pre_data1 <- data_raw %>%
  left_join(batter_atbat_counts) %>%
  left_join(pitcher_atbat_counts) %>%
  mutate(platoon = resp_bat_hand_cd == resp_pit_hand_cd,
         event_cd = as.numeric(event_cd),
         bat_home_id = as.numeric(bat_home_id),
         onbase = as.numeric(bat_dest_id) > 0,
         game_date = ymd(game_dt),
         game_year = year(game_date)) %>%
  filter(resp_bat_id %in% matchup_counts$resp_bat_id,
         resp_pit_id %in% matchup_counts$resp_pit_id,
         bat_event_fl == "true")

batter_years <- pre_data1 %>%
  count(resp_bat_id, game_year) %>%
  group_by(resp_bat_id) %>%
  arrange(game_year) %>%
  mutate(period = row_number())

pitcher_years <- pre_data1 %>%
  count(resp_pit_id, game_year) %>%
  group_by(resp_pit_id) %>%
  arrange(game_year) %>%
  mutate(period = row_number())

batter_periods <- batter_years %>%
  arrange(resp_bat_id) %>%
  group_by(resp_bat_id) %>%
  mutate(max_year = max(game_year),
         min_year = min(game_year)) %>%
  filter(period == max(period)) %>%
  ungroup() %>%
  mutate(total_periods = period)

pitcher_periods <- pitcher_years %>%
  arrange(resp_pit_id) %>%
  group_by(resp_pit_id) %>%
  mutate(max_year = max(game_year),
         min_year = min(game_year)) %>%
  filter(period == max(period)) %>%
  ungroup() %>%
  mutate(total_periods = period)

pre_data2 <- pre_data1 %>%
  left_join(batter_years, by = c("resp_bat_id", "game_year")) %>%
  left_join(pitcher_years, by = c("resp_pit_id", "game_year"),
            suffix = c("_batter", "_pitcher"))

data <- list(
  N = nrow(pre_data2),
  B = length(unique(pre_data2$resp_bat_id)),
  P = length(unique(pre_data2$resp_pit_id)),
  P_B = sum(batter_periods$total_periods),
  P_P = sum(pitcher_periods$total_periods),
  batter_periods = batter_periods$total_periods,
  pitcher_periods = pitcher_periods$total_periods,
  batter_index = as.numeric(factor(pre_data2$resp_bat_id)),
  pitcher_index = as.numeric(factor(pre_data2$resp_pit_id)),
  cumulative_batter_index = c(0, cumsum(head(batter_periods$total_periods,-1)) - 1),
  cumulative_pitcher_index = c(0, cumsum(head(pitcher_periods$total_periods,-1)) - 1),
  period_index_batter = pre_data2$period_batter,
  period_index_pitcher = pre_data2$period_pitcher,
  outcome = pre_data2$bat_dest_id > 0
)


model4 <- stan(file = "stan/model-4.stan",
               data = data,
               iter = 5000,
               chains = 2)

model_4_summary <- rstan::summary(model4)$summary %>%
  as.data.frame()
model_4_summary$parameter <- rownames(model_4_summary)

beta <- model_4_summary %>%
  filter(grepl("beta", parameter),
         !grepl("mu", parameter),
         !grepl("sigma", parameter)) %>%
  mutate(batter = rep(levels(factor(pre_data2$resp_bat_id)),
                      times = data$batter_periods)) %>%
  group_by(batter) %>%
  mutate(period = row_number()) %>%
  left_join(batter_periods %>%
              select(resp_bat_id, min_year),
            by = c("batter" = "resp_bat_id")) %>%
  mutate(year = min_year + period - 1)

theta <- model_4_summary %>%
  filter(grepl("theta", parameter),
         !grepl("mu", parameter),
         !grepl("sigma", parameter)) %>%
  mutate(pitcher = rep(levels(factor(pre_data2$resp_pit_id)),
                      times = data$pitcher_periods)) %>%
  group_by(pitcher) %>%
  mutate(period = row_number()) %>%
  left_join(pitcher_periods %>%
              select(resp_pit_id, min_year),
            by = c("pitcher" = "resp_pit_id")) %>%
  mutate(year = min_year + period - 1)

theta %>%
  ggplot(aes(period, mean, color = pitcher)) +
  geom_line() +
  # geom_errorbar(aes(ymax = `75%`, ymin = `25%`)) +
  theme(legend.position = "none")

beta %>%
  ggplot(aes(period, mean, color = batter)) +
  geom_line() +
  theme(legend.position = "none")



