

source("utils.R")

atbat_2017 <- readRDS("data/atbat-data-2017.rds")
pitch_2017 <- readRDS("data/pitch-data-2017.rds")
game_2017 <- readRDS("data/games-2017.rds")
people <- readRDS("data/people.rds")

# Testing model-2.stan ----

MINIMUM_ATBATS <- 600

sample_games <- sample(unique(atbat_2017$gameday_link), 20)
sample_batters <- atbat_2017 %>%
  count(batter) %>%
  filter(n > MINIMUM_ATBATS)
sample_pitchers <- atbat_2017 %>%
  count(pitcher) %>%
  filter(n > MINIMUM_ATBATS)

pitcher_atbat_counts <- atbat_2017 %>%
  group_by(pitcher) %>%
  summarise(pitcher_count = n())

batter_atbat_counts <- atbat_2017 %>%
  group_by(batter) %>%
  summarise(batter_count = n())

event_counts <- count(atbat_2017, event)

pre_data <- atbat_2017 %>%
  left_join(game_2017 %>%
              select(venue,
                     gameday_link), by = "gameday_link") %>%
  left_join(batter_atbat_counts) %>%
  left_join(pitcher_atbat_counts) %>%
  mutate(platoon = stand == p_throws) %>%
  filter(#month(start_tfs_zulu) > 3,
         #gameday_link %in% sample_games,
         batter %in% sample_batters$batter,
         pitcher %in% sample_pitchers$pitcher#,
         # event %in% (event_counts %>%
         #   filter(n > 500))$event
         ) %>%
  filter(event %in% c("Single", "Double", "Triple", "Strikeout",
                      "Walk", "Groundout", "Flyout", "Pop Out", "Home Run",
                      "Intent Walk", "Lineout")) #%>%
  # filter(event %in% c("Single", "Walk", "Home Run", "Strikeout")) %>%
  sample_n(1500)
  
m_matrix <- model.matrix(
  event ~ platoon,# + venue,
  data = pre_data
)

event_values <- c("Walk" = 0.55,
                  "Strikeout" = 0,
                  "Groundout" = 0,
                  "Lineout" = 0,
                  "Flyout" = 0,
                  "Double" = 1,
                  "Pop Out" = 0,
                  "Single" = 0.7,
                  "Home Run" = 1.65,
                  "Intent Walk" = 0.55,
                  "Triple" = 1.27,
                  "Grounded Into DP" = 0,
                  "Forceout" = 0,
                  "Field Error" = 0,
                  "Hit By Pitch" = 0.55,
                  "Sac Fly" = 0,
                  "Fielders Choice" = 0,
                  "Bunt Lineout" = 0,
                  "Double Play" = 0)

data <- list(
  N = nrow(pre_data),
  P = length(unique(pre_data$pitcher)),
  B = length(unique(pre_data$batter)),
  D = length(unique(pre_data$event)),
  K = ncol(m_matrix),
  batter = as.numeric(factor(pre_data$batter)),
  pitcher = as.numeric(factor(pre_data$pitcher)),
  outcome = as.numeric(factor(pre_data$event)),
  event_values = event_values[names(event_values) %in% unique(pre_data$event)][order(names(event_values[names(event_values) %in% unique(pre_data$event)]))],
  V = m_matrix,
  zero = 0
)


model_2 <- stan(file = "stan/model-2.stan",
                data = data,
                iter = 1000,
                chains = 2,
                control = list(
                  max_treedepth = 18
                ))




# Process results

model2_summary <- summary(model_2)$summary %>%
  as.data.frame() %>%
  mutate(parameter = rownames(.))

beta <- model2_summary %>%
  filter(grepl("pitcher_outcomes", parameter),
         !grepl("mean", parameter)) %>%
  cbind(pitcher = as.numeric(levels(factor(pre_data$pitcher))),
        outcome = rep(levels(factor(pre_data$event)),
                      times = length(unique(pre_data$pitcher)))) %>%
  left_join(people %>%
              select(key_mlbam, name_last, name_first) %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("pitcher" = "key_mlbam"))

theta <- model2_summary %>%
  filter(grepl("batter_outcomes", parameter),
         !grepl("mean", parameter),
         !grepl("mu_theta", parameter),
         !grepl("sigma_theta", parameter),
         !grepl("star", parameter)) %>%
  cbind(batter = rep(as.numeric(levels(factor(pre_data$batter))),
                     each = length(levels(factor(pre_data$event)))),
        outcome = rep(levels(factor(pre_data$event)),
                      times = length(unique(pre_data$batter)))) %>%
  left_join(people %>%
              select(key_mlbam, name_last, name_first) %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("batter" = "key_mlbam"))

omega <- model2_summary %>%
  filter(grepl("omega", parameter),
         !grepl("raw", parameter)) %>%
  mutate(outcome = rep(levels(factor(pre_data$event)),
                       each = data$K),
         variable = rep(colnames(m_matrix),
                        times = data$D))

beta %>%
  ggplot(aes(mean, color = outcome)) +
  geom_density()

pitcher_raw <- pre_data %>%
  group_by(pitcher, event) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pct = n / sum(n)) %>%
  left_join(people %>%
              select(key_mlbam, name_last, name_first) %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("pitcher" = "key_mlbam")) %T>%
  {print(ggplot(., aes(pct, color = event)) +
  geom_density())}

theta %>%
  ggplot(aes(mean, color = outcome)) +
  geom_density()

batter_raw <- pre_data %>%
  group_by(batter, event) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(batter) %>%
  mutate(pct = n / sum(n)) %>%
  left_join(people %>%
              select(key_mlbam, name_last, name_first) %>%
              mutate(key_mlbam = as.numeric(key_mlbam)), by = c("batter" = "key_mlbam")) %T>%
  {print(ggplot(., aes(pct, color = event)) +
           geom_density() +
           facet_wrap(~event))}


theta %>%
  left_join(batter_raw %>%
              select(-n) %>%
              spread(event, pct, fill = 0) %>%
              gather(event, pct, -name_first, -name_last, -batter), by = c("batter",
                               "outcome" = "event")) %>%
  ggplot(aes(mean, pct)) +
  geom_point() +
  geom_abline(data = NULL,
              aes(slope = 1, intercept = 0)) +
  facet_wrap(~outcome)




