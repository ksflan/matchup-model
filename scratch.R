


source("utils.R")



# pitches <- dbGetQuery(con, "select *
#                             from pitchfx.pitch
#                             where date_part('year', tfs_zulu) = 2017")

# players <- dbGetQuery(con, "select * from register.register_people")
# saveRDS(players, "data/people.rds")

# games <- dbGetQuery(con, "select * from pitchfx.schedule where date_part('year', original_date) = 2017")
# saveRDS(games, "data/games-2017.rds")



pitch_counts <- pitch_2017 %>%
  left_join(atbat_2017,
            by = c("gameday_link", "num")) %>%
  group_by(gameday_link, pitcher) %>%
  arrange(num) %>%
  mutate(pitch_count = row_number()) %>%
  ungroup() %>%
  group_by(gameday_link, pitcher, num) %>%
  summarise(pitch_count = min(pitch_count))

pitch_frequencies <- pitch_2017 %>%
  select(gameday_link, num, pitch_type, tfs_zulu, start_speed, spin_rate) %>%
  filter(!(pitch_type %in% c("AB", "", "PO", "UN", "EP", "SC", "FO", "KN", "KC")),
         !is.na(pitch_type),
         month(tfs_zulu) > 3) %>%
  left_join(atbat_2017, by = c("num", "gameday_link")) %>%
  mutate(pitch_category = case_when(
    pitch_type %in% c("SI", "FC", "FF", "FT") ~ "FB",
    pitch_type %in% c("SL", "CH", "CU") ~ "BB"
  )) %>%
  group_by(pitcher, pitch_category) %>%
  summarise(n = n(),
            mean_speed = mean(start_speed),
            mean_spin_rate = mean(spin_rate)
  ) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  filter(!pct == 1) %>%
  select(-n, -mean_speed, -mean_spin_rate) %>%
  spread(pitch_category, pct, fill = 0) %>%
  mutate(fb_bb_ratio = log((FB + 0.0000001) / BB),
         other_bb_ratio = log((`<NA>` + 0.0000001) / BB)) %>%
  filter(!is.infinite(fb_bb_ratio))

#,
#ff_bb_ratio = log(FB / BB),
# pitch_type = paste0(pitch_type, "_freq"),

#log_odds = log(pct / (1 - pct)),

# pitch_characteristics <- pitch_2017 %>%
#   select(gameday_link, num, pitch_type, tfs_zulu, start_speed, spin_rate) %>%
#   filter(!(pitch_type %in% c("AB", "", "PO", "UN", "EP", "SC", "FO", "KN", "KC")),
#          !is.na(pitch_type),
#          month(tfs_zulu) > 3) %>%
#   left_join(atbat_2017, by = c("num", "gameday_link")) %>%
#   group_by(pitcher, pitch_type) %>%
#   summarise(n = n(),
#             mean_speed = mean(start_speed),
#             mean_spin_rate = mean(spin_rate)
#   ) %>%
#   ungroup() %>%
#   group_by(pitcher) %>%
#   mutate(pitch_type = paste0(pitch_type, "_speed"),
#          pct = n/sum(n),
#          log_odds = log(pct / (1 - pct))) %>%
#   ungroup() %>%
#   filter(!pct == 1) %>%
#   select(-n, -log_odds, -mean_speed, -mean_spin_rate) %>%
#   spread(pitch_type, pct, fill = 0)

pitcher_profiles <- pitch_frequencies #%>%
  # left_join(pitch_characteristics,
  #           by = "pitcher")

MINIMUM_ATBATS <- 350

sample_games <- sample(unique(atbat_2017$gameday_link), 20)
sample_batters <- atbat_2017 %>%
  count(batter) %>%
  filter(n > MINIMUM_ATBATS)
sample_pitchers <- atbat_2017 %>%
  count(pitcher) %>%
  filter(n > MINIMUM_ATBATS)


pre_data <- atbat_2017 %>%
  left_join(pitch_counts) %>%
  left_join(game_2017 %>%
              select(gameday_link, venue) %>%
              mutate(venue = as.character(venue)),
            by = c("gameday_link")) %>%
  # sample_n(1000) %>%
  filter(month(start_tfs_zulu) > 3,
  #gameday_link %in% sample_games,
         batter %in% sample_batters$batter,
         pitcher %in% sample_pitchers$pitcher) %>%
  mutate(outcome = ifelse(event %in% "Strikeout", 1, 0), #%in% c("Home Run", "Single", "Double", "Walk", "Triple", "Hit By Pitch", "Intent Walk"), 1, 0),
         event_code = convert_outcomes(event)) %>%
  left_join(pitcher_profiles, by = c("pitcher"))%>%
  filter(!is.na(fb_bb_ratio),
         !is.na(pitch_count))

m_matrix <- model.matrix(
  outcome ~ (p_throws == stand) + (inning_side == "top") + pitch_count + fb_bb_ratio + other_bb_ratio, # + venue
  data = pre_data
) # replace home/away with specific stadiums |||| add pitch count as a predictor

data <- list(
  N = nrow(pre_data),
  B = length(unique(pre_data$batter)),
  P = length(unique(pre_data$pitcher)),
  K = ncol(m_matrix),
  batter = as.numeric(factor(pre_data$batter)),
  pitcher = as.numeric(factor(pre_data$pitcher)),
  outcome = pre_data$outcome,
  V = m_matrix
)

model1 <- stan("model-1.stan",
               data = data,
               iter = 2000,
               chains = 2)

model1_summary <- summary(model1)$summary %>%
  as.data.frame() %>%
  mutate(parameter = rownames(.))

beta <- model1_summary %>%
  filter(grepl("beta", parameter)) %>%
  cbind(pitcher = as.numeric(levels(factor(pre_data$pitcher)))) %>%
  left_join(people %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("pitcher" = "key_mlbam"))

theta <- model1_summary %>%
  filter(grepl("theta", parameter),
         !grepl("mu_theta", parameter),
         !grepl("sigma_theta", parameter),
         !grepl("star", parameter)) %>%
  cbind(batter = rep(as.numeric(levels(factor(pre_data$batter))), each = ncol(m_matrix)),
        variable_id = rep(1:ncol(m_matrix), times = length(unique(pre_data$batter)))) %>%
  left_join(people %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("batter" = "key_mlbam"))








pitch_counts <- pitch_2017 %>%
  left_join(atbat_2017,
            by = c("gameday_link", "num")) %>%
  group_by(gameday_link, pitcher) %>%
  arrange(num) %>%
  mutate(pitch_count = row_number()) %>%
  ungroup() %>%
  group_by(gameday_link, pitcher, num) %>%
  summarise(pitch_count = min(pitch_count))









