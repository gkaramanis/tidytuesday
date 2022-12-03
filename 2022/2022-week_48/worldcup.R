library(tidyverse)
library(svgparser)
library(ggforce)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 320)

worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

football <- read_svg(here::here("2022/2022-week_48/data/football-svgrepo-com.svg"), obj_type = 'data.frame')

wins <- worldcups %>% 
  mutate(winner = if_else(str_detect(winner, "Germany"), "West Germany/Germany", winner)) %>% 
  count(winner, sort = TRUE)
  
mapping <- read_csv(here::here("2020/2020-week52", "data", "mapglyphs-mapping.csv"))

wins_ball <- wins %>% 
  add_row(winner = " ", n = 99) %>% 
  mutate(
    name = case_when(
      winner == "West Germany/Germany" ~ "Germany",
      # winner == "England" ~ "United Kingdom",
      TRUE ~ winner
    ),
    winner = fct_reorder(winner, -n)
    ) %>% 
  left_join(mapping, by = c("name" = "country")) %>% 
  rowwise() %>% 
  mutate(idx = list(1:12)) %>% 
  mutate(idx_black = list(c(7, 4, 2, 6, 9, 11)[1:(n + 1)])) %>% 
  ungroup() %>% 
  unnest(idx) %>% 
  rowwise() %>% 
  mutate(idx_fill = if_else(idx %in% idx_black | idx == 1, "black", "white")) %>% 
  ungroup() %>% 
  left_join(football, by = "idx")

f1 <- "Outfit"

ggplot(wins_ball %>% filter(idx != 1 & n != 99)) + 
  geom_circle(data = NULL, aes(x0 = 32, y0 = 32, r = 32), n = 72, fill = "black", color = NA) +
  geom_shape(aes(x = x, y = y, group = interaction(idx, winner), fill = idx_fill), color = "black", expand = 0.01, linewidth = 0.2) +
  geom_text(aes(x = 32, y = 32, label = icon), color = "white", family = "MapGlyphs", size = 18, stat = "unique") +
  geom_text(aes(x = 32, y = 22, label = n), color = "white", fontface = "bold", family = f1, size = 4, stat = "unique") +
  geom_text(data = wins_ball %>% filter(n == 99), aes(x = 0, y = 60, label = "World\nCup\nWinners"), family = f1, size = 20, hjust = 0, vjust = 1, fontface = "bold", lineheight = 0.8, color = "#d4af37") +
  geom_text(data = wins_ball %>% filter(n == 99), aes(x = 1, y = 13, label = "Source: FIFA World Cup\nGraphic: Georgios Karamanis"), family = f1, size = 5, hjust = 0, vjust = 1, color = "grey60", lineheight = 1) +
  scale_fill_identity() +
  facet_wrap(vars(winner)) +
  coord_fixed() +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_text(size = 20, margin = margin(5, 0, 5, 0), face = "bold")
  )
  

