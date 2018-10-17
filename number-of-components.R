
library(igraph)

high_weight <- atbat_2017 %>%
  count(batter, pitcher) %>%
  filter(n > 17)

edge_table <- data_raw %>%
  filter(as.numeric(substr(game_dt, 1, 4)) == 2017,
         resp_pit_id %in% matchup_counts$resp_pit_id,
         resp_bat_id %in% matchup_counts$resp_bat_id,
         bat_event_fl == "true") %>%
  
  count(a = resp_bat_id, b = resp_pit_id) %>%
  rbind(
    data_raw %>%
      filter(as.numeric(substr(game_dt, 1, 4)) == 2017,
             resp_pit_id %in% matchup_counts$resp_pit_id,
             resp_bat_id %in% matchup_counts$resp_bat_id,
             bat_event_fl == "true") %>%
      count(a = resp_bat_id, b = park_id)
  ) %>%
  rbind(
    data_raw %>%
      filter(as.numeric(substr(game_dt, 1, 4)) == 2017,
             resp_pit_id %in% matchup_counts$resp_pit_id,
             resp_bat_id %in% matchup_counts$resp_bat_id,
             bat_event_fl == "true") %>%
      count(a = resp_pit_id, b = park_id)
  )


edge_list <- edge_table %>%
  select(-n) %>%
  as.matrix()
edge_list <- apply(edge_list, 2, FUN = as.character)

g <- graph_from_edgelist(edge_list, directed = F)
igraph::edge_attr(g, "weight") <- edge_table$n
components <- components(g)

e <- eigen(laplacian_matrix(g))
sum(e$values == 0)

get.adjacency(g)
clique.number(g)
ge <- E(g)
ge$weight
