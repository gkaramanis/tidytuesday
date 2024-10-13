library(tidyverse)
library(bigchess)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

chess_lan <- chess %>% 
  head(500) %>% 
  rowwise() %>% 
  mutate(lan = san2lan(moves)) %>% 
  ungroup() %>% 
  mutate(lan = str_split(lan, " ")) %>% 
  unnest(lan)
  
chess_xy <- chess_lan %>% 
  select(game_id, lan) %>% 
  mutate(
    x1 = str_sub(lan, 1, 1), 
    y1 = str_sub(lan, 2, 2), 
    x2 = str_sub(lan, 3, 3), 
    y2 = str_sub(lan, 4, 4)
  )

chess_xy %>% 
  count(x1, y1) %>% 
  ggplot() +
  geom_tile(aes(x = x1, y = y1, fill = n)) +
  coord_fixed()


chess_xy %>% 
  count(x1, y1, x2, y2) %>% 
  ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.1) +
  scale_linewidth_continuous(range = c(0.1, 4)) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )
