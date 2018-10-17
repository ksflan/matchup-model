
# data_raw <- dbGetQuery(con, "select e.*, g.park_id, g.game_dt, g.home_team_id
# from retrosheet.events e
#                        left join retrosheet.games g on e.game_id = g.game_id
#                        where left(cast(game_dt as text), 4) > '2016'")
# saveRDS(data_raw %>%
#           filter(substr(game_dt, 1, 4) == "2017"), "data/raw-retrosheet-17.rds")
# #
# write_data <- data_raw %>%
#   select(game_id, away_team_id, home_team_id, inn_ct, bat_home_id, outs_ct, resp_bat_id, resp_bat_hand_cd,
#          resp_pit_id, resp_pit_hand_cd, event_cd, event_id, bat_event_fl, ab_fl, game_dt, park_id, bat_dest_id)
# saveRDS(write_data, "data/retrosheet-10-17.rds")

source("utils.R")


# Building model with retrosheet data ----

data_raw <- readRDS("data/retrosheet-17.rds")
event_values <- readRDS("retrosheet-event-values-1980-2017.rds") %>%
  mutate(event_cd = as.numeric(event_cd))
people <- readRDS("data/people.rds")

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
  filter(n > 24)


# predata

pre_data <- data_raw %>%
  mutate(event_cd = as.numeric(event_cd)) %>%
  left_join(batter_atbat_counts) %>%
  left_join(pitcher_atbat_counts) %>%
  left_join(event_codes, by = c("event_cd" = "code")) %>%
  mutate(platoon = resp_bat_hand_cd == resp_pit_hand_cd,
         event_cd = as.numeric(event_cd),
         bat_home_id = as.numeric(bat_home_id)) %>%
  filter(#month(start_tfs_zulu) > 3,
    #gameday_link %in% sample_games,
    bat_event_fl == "true",
    as.numeric(substr(game_dt, 1, 4)) == 2017,
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

pre_batter <- positions %>%
  filter(id %in% pre_data$resp_bat_id) %>%
  arrange(id)


data <- list(
  N = nrow(pre_data),
  P = length(unique(pre_data$resp_pit_id)),
  B = length(unique(pre_data$resp_bat_id)),
  D = length(unique(pre_data$event_cd)),
  # K = ncol(m_matrix),
  F = length(unique(pre_batter$position)),
  batter = as.numeric(factor(pre_data$resp_bat_id)),
  pitcher = as.numeric(factor(pre_data$resp_pit_id)),
  outcome = as.numeric(factor(pre_data$event_cd)),#case_when(pre_data$event_cd == 27 ~ 1,
                      # pre_data$event_cd %in% c(2,3,18,19) ~ 2,
                      # 1 == 1 ~ 3),
  event_values = event_values$avg_value[event_values$event_cd %in% pre_data$event_cd &
                                         event_values$year == 2017][order(event_values$event_cd[event_values$event_cd %in% pre_data$event_cd &
                                 event_values$year == 2017])],
  # V = m_matrix,
  batter_position = as.numeric(factor(pre_batter$position)),
  venue_matrix = venue_matrix,
  home_advantage = pre_data$bat_home_id == 1,
  platoon = pre_data$resp_bat_hand_cd == pre_data$resp_pit_hand_cd,
  S = length(unique(pre_data$park_id)),
  stadium = as.numeric(factor(pre_data$park_id)),
  zero = 0
)


model_2_wOBA <- stan(file = "stan/model-2.stan",
                     data = data,
                     iter = 1000,
                     chains = 2,
                     control = list(
                       max_treedepth = 13
                     ))

model5 <- stan(file = "stan/model-5.stan",
               data = data,
               iter = 1000,
               chains = 2,
               control = list(
                 max_treedepth = 13
               ))

model6 <- stan(file = "stan/model-6.stan",
               data = data,
               iter = 1000,
               chains = 2,
               control = list(
                 max_treedepth = 13
               ))


model_summary <- rstan::summary(model6)$summary %>%
  as.data.frame() %>%
  mutate(parameter = rownames(rstan::summary(model6)$summary))

batter_outcomes <- model_summary %>%
  filter(grepl("batter_outcomes", parameter),
         !grepl("mean_batter_outcomes", parameter)) %>%
  mutate(batter = rep(levels(factor(pre_data$resp_bat_id)),
                      each = data$D),
         outcome = as.numeric(rep(levels(factor(pre_data$event_cd)),
                       times = data$B))) %>%
  left_join(event_codes,
            by = c("outcome" = "code")) %>%
  left_join(people %>%
              select(key_retro, name_first, name_last),
            by = c("batter" = "key_retro")) %>%
  mutate(display_name = paste0(name_first, " ", name_last))

pitcher_outcomes <- model_summary %>%
  filter(grepl("pitcher_outcomes", parameter),
         !grepl("mean_pitcher_outcomes", parameter)) %>%
  mutate(pitcher = rep(levels(factor(pre_data$resp_pit_id)),
                      each = data$D),
         outcome = as.numeric(rep(levels(factor(pre_data$event_cd)),
                                  times = data$P))) %>%
  left_join(event_codes,
            by = c("outcome" = "code")) %>%
  left_join(people %>%
              select(key_retro, name_first, name_last),
            by = c("pitcher" = "key_retro")) %>%
  mutate(display_name = paste0(name_first, " ", name_last))

stadium_outcomes <- model_summary %>%
  filter(grepl("stadium_outcomes", parameter),
         !grepl("mean_stadium_outcomes", parameter)) %>%
  mutate(stadium = rep(levels(factor(pre_data$park_id)),
                       each = data$D),
         outcome = as.numeric(rep(levels(factor(pre_data$event_cd)),
                                  times = data$S))) %>%
  left_join(event_codes,
            by = c("outcome" = "code"))

position_outcomes <- model_summary %>%
  filter(grepl("position_outcomes", parameter),
         !grepl("mean_stadium_outcomes", parameter)) %>%
  mutate(position = rep(levels(factor(pre_batter$position)),
                        each = data$D),
         outcome = as.numeric(rep(levels(factor(pre_data$event_cd)),
                                  times = data$F))) %>%
  left_join(event_codes,
            by = c("outcome" = "code"))

batter_woba <- model_summary %>%
  filter(grepl("batter_wOBA", parameter)) %>%
  mutate(batter = levels(factor(pre_data$resp_bat_id))) %>%
  left_join(people %>%
              select(key_retro, name_first, name_last),
            by = c("batter" = "key_retro")) %>%
  mutate(display_name = paste0(name_first, " ", name_last))

pitcher_woba <- model_summary %>%
  filter(grepl("pitcher_wOBA", parameter)) %>%
  mutate(pitcher = levels(factor(pre_data$resp_pit_id))) %>%
  left_join(people %>%
              select(key_retro, name_first, name_last),
            by = c("pitcher" = "key_retro")) %>%
  mutate(display_name = paste0(name_first, " ", name_last))

stadium_woba <- model_summary %>%
  filter(grepl("stadium_wOBA", parameter)) %>%
  mutate(stadium = levels(factor(pre_data$park_id)))

## Plots ----

batter_outcomes %>%
  ggplot(aes(display_name, mean)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `2.5%`, yend = `97.5%`)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `25%`, yend = `75%`),
               color = "red", size = 1.2) +
  geom_point() +
  coord_flip() +
  facet_wrap(~label) +
  theme_minimal() +
  labs(x = "")

pitcher_outcomes %>%
  ggplot(aes(display_name, mean)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `2.5%`, yend = `97.5%`)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `25%`, yend = `75%`),
               color = "red", size = 1.2) +
  geom_point() +
  coord_flip() +
  facet_wrap(~label) +
  theme_minimal() +
  labs(x = "")

stadium_outcomes %>%
  ggplot(aes(stadium, mean)) +
  geom_segment(aes(x = stadium, xend = stadium,
                   y = `2.5%`, yend = `97.5%`)) +
  geom_segment(aes(x = stadium, xend = stadium,
                   y = `25%`, yend = `75%`),
               color = "red", size = 1.2) +
  geom_point() +
  coord_flip() +
  facet_wrap(~label) +
  theme_minimal() +
  labs(x = "")


batter_woba %>%
  ggplot(aes(display_name, mean)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `2.5%`, yend = `97.5%`)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `25%`, yend = `75%`),
               color = "red", size = 1.2) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(x = "")

pitcher_woba %>%
  ggplot(aes(display_name, mean)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `2.5%`, yend = `97.5%`)) +
  geom_segment(aes(x = display_name, xend = display_name,
                   y = `25%`, yend = `75%`),
               color = "blue", size = 1.2) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(x = "")








