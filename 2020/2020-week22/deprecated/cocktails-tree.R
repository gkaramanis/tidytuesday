library(tidyverse)
library(ggraph)
library(igraph)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

iba_cocktails <- cocktails %>% 
  filter(!is.na(iba))

d1 <- data.frame(from = "cocktails", to = unique(iba_cocktails$drink))

d2 <- iba_cocktails %>% 
  select(from = drink, to = ingredient)

hierarchy <- rbind(d1, d2)

vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 

vertices$group  <- hierarchy$from[match(vertices$name, hierarchy$to)]
vertices$iba <- iba_cocktails$iba[match(vertices$group, iba_cocktails$drink)]

mygraph <- graph_from_data_frame(hierarchy, vertices = vertices)

connect <- d2 %>%
  group_by(from) %>% 
  mutate(to2 = lag(to)) %>% 
  ungroup() %>% 
  select(from = to2, to)

from <- match(connect$from, vertices$name)  
to <- match(connect$to, vertices$name)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), aes(colour = group), alpha = 0.2, tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x * 1.05, y = y * 1.05)) +
  facet_nodes(vars(group), nrow = 4) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

ggsave(here::here("2020-week22", "plots", "temp", paste0("cocktails-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)



