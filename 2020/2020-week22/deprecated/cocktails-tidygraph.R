library(tidyverse)
library(tidygraph)
library(ggraph)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

iba_cocktails <- cocktails %>% 
  filter(!is.na(iba)) %>%
  # group_by(drink) %>% 
  # filter(any(ingredient == "Dark rum")) %>% 
  # ungroup() %>% 
  select(drink, iba, from = ingredient) %>% 
  group_by(drink) %>% 
  mutate(
    from = tolower(from),
    to = lag(from)
    ) %>% 
  ungroup() %>% 
  filter(!is.na(to))

iba_graph <- as_tbl_graph(iba_cocktails) %>% 
  activate(nodes) %>% 
  arrange(name) %>% 
  mutate(
    id = row_number(),
    angle = 90 - 360 * id / max(id),
    hjust = ifelse(angle < -90, 1, 0),
    angle = ifelse(angle < -90, angle + 180, angle)
    )

ggraph(iba_graph, layout = "kk") +
  geom_node_point(alpha = 0.2) +
  geom_edge_fan() +
  # geom_edge_arc(alpha = 0.2) +
  # geom_node_text(aes(x = x * 1.05, y = y * 1.05, label = name, angle = angle, hjust = hjust), family = "IBM Plex Mono", size = 3) +
  expand_limits(x = c(-1.4, 1.4), y = c(-1.4, 1.4)) +
  facet_edges(vars(drink)) +
  coord_fixed() +
  theme_void(base_family = "IBM Plex Mono") 

ggsave(here::here("2020-week22", "plots", "temp", paste0("cocktails-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 18, height = 10)

