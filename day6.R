# Day 6

library(igraph)
# demo(package = "igraph")
help(package = "igraph")

edges_test <-
  readr::read_delim("day6-test-map.txt", delim = ")", col_names = FALSE) 
vertices_test <-
  unique(c(edges_test$X1, edges_test$X2))
length(vertices_test) # 12

g_test <- graph_from_data_frame(edges_test, directed = TRUE, vertices = vertices_test)
vcount(g_test) # 12
ecount(g_test) # 11
distance_table(g_test)$res %>% sum()

edges <-
  readr::read_delim("day6-map.txt", delim = ")", col_names = FALSE) 
vertices <-
  unique(c(edges$X1, edges$X2))
g <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)
distance_table(g)$res %>% sum()

## Part 2
# Orbital transfers

orbits <-
  readr::read_delim("day6-test-map-part2.txt", delim = ")", col_names = FALSE) 
objects <-
  unique(c(orbits$X1, orbits$X2))
length(objects) # 12

grph <- graph_from_data_frame(orbits, directed = TRUE, vertices = objects)

distances(
  grph, 
  v = V(grph)[["YOU"]], 
  to = V(grph)[["SAN"]]
) - 2  # 4 


orbits <-
  readr::read_delim("day6-map.txt", delim = ")", col_names = FALSE) 
objects <-
  unique(c(orbits$X1, orbits$X2))
length(objects) # 1605
"SAN" %in% objects
"YOU" %in% objects

grph <- graph_from_data_frame(orbits, directed = TRUE, vertices = objects)

distances(
  grph, 
  v = V(grph)[["YOU"]], 
  to = V(grph)[["SAN"]]
) - 2  # 475 

