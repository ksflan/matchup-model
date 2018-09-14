

source("utils.R")

atbat_2017 <- readRDS("data/atbat-data-2017.rds")
data_raw <- dbGetQuery(con, "select e.event_id, e.bat_dest_id, e.run1_dest_id, e.event_cd, e.base1_run_id, e.base2_run_id, e.base3_run_id,
                            e.run2_dest_id, e.run3_dest_id, e.outs_ct, e.game_id, e.inn_ct, e.bat_home_id,
                           g.game_dt from retrosheet.events e left join retrosheet.games g on e.game_id = g.game_id where left(cast(game_dt as text), 4) > '1980'")
# data2011_2012_raw <- dbGetQuery(con, "select e.*, g.game_dt from retrosheet.events e left join retrosheet.games g on e.game_id = g.game_id where left(cast(game_dt as text), 4) = '2011' or left(cast(game_dt as text), 4) = '2012'")

runs <- data_raw %>%
  # select(game_id, inn_ct, bat_home_id, home_score_ct, away_score_ct, rbi_ct,
  #        bat_dest_id, run1_dest_id, run2_dest_id, run3_dest_id, event_cd) %>%
  mutate(year = as.numeric(substr(game_dt, 1, 4)),
         event_id = as.numeric(event_id),
         score_ct = (bat_dest_id > 3) +
           (run1_dest_id > 3) +
           (run2_dest_id > 3) +
           (run3_dest_id > 3),
         pre_state = paste0(
           as.numeric(!is.na(base1_run_id)),
           as.numeric(!is.na(base2_run_id)),
           as.numeric(!is.na(base3_run_id)),
           " ",
           outs_ct
         )) %>%
  group_by(game_id, inn_ct, bat_home_id) %>%
  dplyr::arrange(event_id) %>%
  dplyr::mutate(post_state = lead(pre_state),
                post_state = ifelse(is.na(post_state), "000 3", post_state),
                cumulative_runs = cumsum(score_ct),
                runs_remaining = max(cumulative_runs) - cumulative_runs)

run_values <- runs %>%
  # head(1000) %>%
  group_by(year, state = pre_state) %>%
  summarise(n = n(),
            avg_value = mean(runs_remaining + score_ct),
            sd_value = sd(runs_remaining + score_ct)) %>%
  bind_rows(data.frame(
    year = unique(.$year),
    state = "000 3",
    n = NA,
    avg_value = 0,
    sd_value = NA,
    stringsAsFactors = FALSE
  )) #%T>%
  # separate(pre_state, c("bases", "outs"), sep = " ") %>%
  # select(-n, -sd_value) %>%
  # spread(outs, avg_value)

event_values <- runs %>%
  # head(1000000) %>%
  ungroup() %>%
  select(event_cd, score_ct, pre_state, post_state, year) %>%
  left_join(run_values,
            by = c("pre_state" = "state", "year")) %>%
  left_join(run_values,
            by = c("post_state" = "state", "year"),
            suffix = c("_pre", "_post")) %>%
  mutate(run_differential = score_ct + avg_value_post - avg_value_pre) %>%
  group_by(year, event_cd) %>%
  summarise(n = n(),
            avg_value = mean(run_differential),
            sd_value = sd(run_differential))

event_values %>%
  ggplot(aes(year, avg_value, color = as.character(event_cd))) +
  geom_line() +
  facet_wrap(~event_cd)

# runs %>%
#   group_by(state) %>%
#   dplyr::summarise(n = n(),
#                    avg_value = mean(score_ct + runs_remaining)) %>%
#   View()






