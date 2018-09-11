
source("utils.R")




atbat_2017 <- readRDS("data/atbat-data-2017.rds")
pitch_2017 <- readRDS("data/pitch-data-2017.rds")
game_2017 <- readRDS("data/games-2017.rds")
people <- readRDS("data/people.rds")



pitch_frequencies <- pitch_2017 %>%
  select(gameday_link, num, pitch_type, tfs_zulu, start_speed, spin_rate) %>%
  filter(!(pitch_type %in% c("AB", "", "PO", "UN", "EP", "SC", "FO", "KN", "KC")),
         !is.na(pitch_type),
         month(tfs_zulu) > 3) %>%
  left_join(atbat_2017, by = c("num", "gameday_link")) %>%
  group_by(pitcher, pitch_type) %>%
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
  spread(pitch_type, pct, fill = 0)



alr(pitch_frequencies[,-1])


pitcher_data <- pitch_2017 %>%
  filter(pitch_type != "") %>%
  left_join(atbat_2017,
            by = c("num", "gameday_link")) %>%
  group_by(pitcher, pitch_type) %>%
  summarise(mean_speed = mean(start_speed, na.rm = T),
            mean_spin = mean(spin_rate, na.rm = T),
            mean_x0 = mean(abs(x0), na.rm = T),
            mean_z0 = mean(z0, na.rm = T),
            n = n()) %>%
  group_by(pitcher) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  mutate(mean_spin_z = (mean_spin - mean(mean_spin)) / sd(mean_spin)) %>%
  gather(variable, value, -pitcher, -pitch_type)

pitcher_data %>%
  filter(pitch_type %in% c("FF","FT","CH","CU","SL","FS")) %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_grid(pitch_type ~ variable, scales = "free")

pitcher_data %>%
  filter(pitch_type %in% c("FF", "CU")) %>%
  # select(-pitch_type) %>%
  spread(variable, value) %>%
  select(-pitcher) %>%
  ggpairs(aes(color = pitch_type, alpha = 0.5))

