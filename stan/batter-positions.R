


source("utils.R")

# games <- dbGetQuery(con, "select * from retrosheet.games
#                     where left(cast(game_dt as text), 4) = '2017'")

positions <- data_raw %>%
  mutate(bat_fld_cd = as.numeric(bat_fld_cd)) %>%
  count(resp_bat_id, bat_fld_cd) %>%
  ungroup() %>%
  group_by(resp_bat_id) %>%
  filter(n == max(n)) %>%
  mutate(rnum = row_number()) %>%
  filter(rnum == min(rnum))



# positions <- games %>%
#   select(game_id,
#          matches("fld_cd")) %>%
#   gather(lineup_spot, position, -game_id) %>%
#   mutate(lineup_spot = as.numeric(str_replace_all(
#     lineup_spot, "[^1-9]", ""))
#   ) %>%
#   left_join(
#     games %>%
#       select(game_id,
#              matches("bat_id")) %>%
#       gather(lineup_spot, id, -game_id) %>%
#       mutate(lineup_spot = as.numeric(str_replace_all(
#         lineup_spot, "[^1-9]", ""))
#       )
#   ) %>%
#   count(id, position) %>%
#   ungroup() %>%
#   group_by(id) %>%
#   filter(n == max(n)) %>%
#   mutate(rnum = row_number()) %>%
#   filter(rnum == min(rnum))

# gsub("^[1-9]*", "", positions$lineup_spot)
# str_replace_all(positions$lineup_spot, "[^1-9]", "")
