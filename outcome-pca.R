

batter_outcomes <- atbat_2017 %>%
  mutate(event_code = convert_outcomes(event)) %>%
  count(batter, event_code) %>%
  group_by(batter) %>%
  mutate(pct = n/sum(n)) %>%
  select(-n) %>%
  filter(!is.na(event_code)) %>%
  spread(event_code, pct, fill = 0)

events <- pre_data %>%
  count(batter, event_code) %>%
  spread(event_code, n, fill = 0.0001)


pairs(batter_outcomes[,-1],pch=".")
pairs(alr(batter_outcomes[,-1]),pch=".")

pca <- prcomp(as.matrix(batter_outcomes[,-1]))



