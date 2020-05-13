library(tidyverse)
library(tidytext)
library(geofacet)

villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

villagers_chars <- villagers %>% 
  unnest_tokens(code, name, "characters") %>% 
  group_by(code) %>% 
  summarise(n = n())

qwerty_grid <- data.frame(
  row = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  col = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "z", "x", "c", "v", "b", "n", "m", ",", ".", "/"),
  name = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "z", "x", "c", "v", "b", "n", "m", ",", ".", "/"),
  stringsAsFactors = FALSE
)

ggplot(villagers_chars, aes(x = 0, y = 0)) +
  geom_tile(aes(alpha = n), fill = "#E3C089") +
  geom_text(aes(label = toupper(code)), family = "JetBrains Mono Bold", colour = "white", size = 7) +
  coord_fixed() +
  theme_void(base_family = "JetBrains Mono") +
  facet_geo(~code, grid = qwerty_grid) +
  theme(
    plot.background = element_rect(fill = "#61A554", colour = "#61A554"),
    strip.text = element_blank()
  )
