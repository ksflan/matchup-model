
# data_raw <- dbGetQuery(con, "select e.*, g.park_id, g.game_dt, g.home_team_id
# from retrosheet.events e
#                        left join retrosheet.games g on e.game_id = g.game_id
#                        where left(cast(game_dt as text), 4) > '2016'")
# saveRDS(data_raw, "data/raw-retrosheet-16-17.rds")
# 
# write_data <- data_raw %>%
#   select(game_id, away_team_id, home_team_id, inn_ct, bat_home_id, outs_ct, resp_bat_id, resp_bat_hand_cd,
#          resp_pit_id, resp_pit_hand_cd, event_cd, event_id, bat_event_fl, ab_fl, game_dt, park_id, bat_dest_id)
# saveRDS(write_data, "data/retrosheet-16-17.rds")

source("utils.R")


# Building model with retrosheet data ----

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
  filter(n > 17)


# predata

pre_data <- data_raw %>%
  left_join(batter_atbat_counts) %>%
  left_join(pitcher_atbat_counts) %>%
  mutate(platoon = resp_bat_hand_cd == resp_pit_hand_cd,
         event_cd = as.numeric(event_cd),
         bat_home_id = as.numeric(bat_home_id)) %>%
  filter(#month(start_tfs_zulu) > 3,
    #gameday_link %in% sample_games,
    bat_event_fl == "true",
    resp_bat_id %in% matchup_counts$resp_bat_id,
    resp_pit_id %in% matchup_counts$resp_pit_id#,
    # event %in% (event_counts %>%
    #   filter(n > 500))$event
  )# %>%
  # filter(event %in% c("Single", "Double", "Triple", "Strikeout",
  #                     "Walk", "Groundout", "Flyout", "Pop Out", "Home Run",
  #                     "Intent Walk", "Lineout", "Hit By Pitch")) #%>%
# filter(event %in% c("Single", "Walk", "Home Run", "Strikeout")) %>%
# sample_n(10000)

venue_matrix <- model.matrix(
  event_cd ~ park_id + 0,
  data = pre_data
)




data <- list(
  N = nrow(pre_data),
  P = length(unique(pre_data$resp_pit_id)),
  B = length(unique(pre_data$resp_bat_id)),
  D = length(unique(pre_data$event_cd)),
  # K = ncol(m_matrix),
  batter = as.numeric(factor(pre_data$resp_bat_id)),
  pitcher = as.numeric(factor(pre_data$resp_pit_id)),
  outcome = as.numeric(factor(pre_data$event_cd)),
  event_values = event_values$avg_value[event_values$event_cd %in% pre_data$event_cd &
                                         event_values$year == 2017][order(event_values$event_cd[event_values$event_cd %in% pre_data$event_cd &
                                 event_values$year == 2017])],
  # V = m_matrix,
  venue_matrix = venue_matrix,
  home_advantage = pre_data$bat_home_id == 1,
  platoon = pre_data$resp_bat_hand_cd == pre_data$resp_pit_hand_cd,
  S = length(unique(pre_data$park_id)),
  zero = 0
)


model_2_wOBA <- stan(file = "stan/model-2.stan",
                     data = data,
                     iter = 1000,
                     chains = 2,
                     control = list(
                       max_treedepth = 18
                     ))


