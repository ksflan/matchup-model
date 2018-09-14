
high_weight <- atbat_2017 %>%
  count(batter, pitcher) %>%
  filter(n > 17)


edge_list <- atbat_2017[,c("batter","pitcher")] %>% count(batter, pitcher) %>% select(-n) %>% as.matrix()
edge_list <- apply(edge_list, 2, FUN = as.character)

g <- graph_from_edgelist(edge_list, directed = F)
igraph::edge_attr(g, "weight") <- (atbat_2017[,c("batter","pitcher")] %>% count(batter, pitcher))$n
components <- components(g)

e <- eigen(laplacian_matrix(g))
sum(e$values == 0)

get.adjacency(g)
clique.number(g)
ge <- E(g)
ge$weight

g2 <- delete_edges()
