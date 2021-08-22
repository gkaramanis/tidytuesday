library(tidyverse)
library(here)
library(ggraph)
library(tidygraph)


horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

hm <- horror_movies %>% 
  mutate(cast = str_split(cast, "\\|")) %>% 
  sample_n(1000) %>% 
  select(title, cast) %>% 
  unnest(cols = c(cast))

title <- hm %>%
  distinct(title) %>%
  rename(label = title)

actor <- hm %>%
  distinct(cast) %>%
  rename(label = cast)

nodes <- full_join(title, actor, by = "label")

nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())

edges <- hm %>% 
  left_join(nodes, by = c("title" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("cast" = "label")) %>% 
  rename(to = id)

edges$weight = 1

edges <- select(edges, from, to, weight)

net.tidy <- tbl_graph(
  nodes = nodes, edges = edges, directed = TRUE
)

ggraph(net.tidy, layout = "graphopt") + 
  # geom_node_point(size = 0.5) +
  geom_edge_link(alpha = 0.8) + 
  theme_graph()

  
