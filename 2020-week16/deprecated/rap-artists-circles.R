library(tidyverse)
library(janitor)
library(ggforce)
library(ggtext)

rankings <- read_csv(here::here("2020-week16", "data", "data-kOlSQ.csv")) %>% 
  clean_names() %>% 
  select(title_artist = song_artist, img = spotify_thumb_sm, 3:15) %>% 
  mutate(
    title_artist = str_remove(title_artist, "!\\[\\].+"),
    title_artist = str_replace(title_artist, "\\*\\* ", "\\*\\*"),
    img = ifelse(str_detect(img, "data"), NA, img)
  )

timeline <- rankings %>% 
  # filter(n>1) %>%
  group_by(year) %>% 
  add_count(name = "year_total") %>% 
  mutate(
    year_n = row_number(),
  ) %>% 
  ungroup()

ggplot(timeline) +
  geom_arc_bar(aes(x0 = year, y0 = -year_n, r0 = 0.4, r = 0.1, start = 0, end = 2 * pi, alpha = n1, fill = artist), colour = NA) +
  geom_arc_bar(aes(x0 = year, y0 = -year_n, r0 = 0.3, r = 0.1, start = 0, end = 2 * pi, alpha = n2, fill = artist), colour = NA) +
  geom_arc_bar(aes(x0 = year, y0 = -year_n, r0 = 0.2, r = 0.1, start = 0, end = 2 * pi, alpha = n3, fill = artist), colour = NA) + 
  geom_arc_bar(aes(x0 = year, y0 = -year_n, r0 = 0.1, r = 0.1, start = 0, end = 2 * pi, alpha = n4, fill = artist), colour = NA) + 
  geom_arc_bar(aes(x0 = year, y0 = -year_n, r0 = 0, r = 0.1, start = 0, end = 2 * pi, alpha = n5, fill = artist), colour = NA) +
  scale_x_continuous(position = "top") +
  scale_alpha(range = c(0.1, 1)) +
  scale_fill_viridis_d(option = "inferno") +
  coord_fixed() +
  theme_void(base_family = "IBM Plex Mono") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey20", colour = NA),
    axis.text.x = element_text(size = 15, colour = "grey80", family = "IBM Plex Mono Bold")
  ) +
  ggsave(here::here("2020-week16", "plots", "temp", "rap-artists-circles.png"), dpi = 320, width = 16, height = 10)
