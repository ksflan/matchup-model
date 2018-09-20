





source("utils.R")



# Code for running OBP model

data_raw <- readRDS("data/retrosheet-16-17.rds")
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
  filter(n > 13)


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







