


pitcher_profiles <- pitch_2017 %>%
  select(gameday_link, num, pitch_type, tfs_zulu, start_speed, spin_rate) %>%
  filter(!(pitch_type %in% c("AB", "", "PO", "UN", "EP", "SC", "FO", "KC", "KN")),
         !is.na(pitch_type),
         month(tfs_zulu) > 3) %>%
  left_join(atbat_2017, by = c("num", "gameday_link")) %>%
  group_by(pitcher, pitch_type) %>%
  summarise(n = n()
            #mean_speed = mean(start_speed),
            #mean_spin_rate = mean(spin_rate)
  ) %>%
  ungroup() %>%
  group_by(pitcher) %>%
  mutate(pct = n/sum(n),
         log_odds = log(pct / (1 - pct))) %>%
  ungroup() %>%
  filter(!pct == 1) %>%
  select(-n, -log_odds) %>%
  spread(pitch_type, pct, fill = 0)




pitcher_pca <- prcomp(as.matrix(pitcher_profiles[,-1]))
plot(pitcher_pca$sdev)


corrplot::corrplot(cor(pitcher_profiles[,-1]))











