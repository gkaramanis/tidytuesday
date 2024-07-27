library(tidyverse)
library(ggcirclepack)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11.5, height = 8, units = "in", dpi = 320)

songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/songs.csv')

song_artists <- songs %>%
  distinct(season, week, song, artist) %>% 
  mutate(season = parse_number(season)) %>%
  count(season, artist) %>% 
  group_by(season) %>% 
  # mutate(is_top = if_else(n > max(n) * 0.75, TRUE, FALSE)) %>% 
  mutate(is_top = if_else(n >= quantile(n, 0.99)[[1]], TRUE, FALSE)) %>% 
  ungroup()

top_artists <- song_artists %>% 
  count(artist, sort = TRUE) %>% 
  slice_max(order_by = n, n = 10)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- c(
  "#911eb4",
  "#42d4f4",
  "#fabed4"
)

cc <- scales::seq_gradient_pal(pal)(seq(0, 1, length.out = 18))

ggplot(song_artists, aes(id = artist, fill = if_else(is_top, n, NA), area = n)) +
  geom_circlepack() +
  geom_circlepack_text(aes(label = n, color = if_else(is_top, "white", NA)), family = f1, fontface = "bold") +
  # Top 10 artists but not season top
  geom_circlepack_text(aes(label = str_wrap(artist, 8), color = if_else(!is_top & artist %in% top_artists$artist, "#57504d", NA)), family = f1b, fontface = "bold", lineheight = 0.7, alpha = 0.8) +
  # Season top artists
  geom_circlepack_text(aes(y = after_stat(y - radius * 1.5), label = str_wrap(artist, 8), color = if_else(is_top, "#181716", NA)), family = f1b, fontface = "bold", lineheight = 0.7) +
  scale_color_identity() +
  # scale_fill_manual(values = cc, na.value = "#D9D9E1") +
  scale_fill_gradientn(colors = pal,  na.value = "#D9D9E1") +
  coord_fixed(clip = "off", expand = FALSE) +
  facet_wrap(vars(season), nrow = 3, labeller = labeller(season = function(season) {paste("Season", season)})) +
  labs(
    title = "American Idol's most popular artists",
    subtitle = str_wrap("Circle size represents the number of times an artist’s songs were performed, based on unique artist-song combinations. Featuring the names of the most popular artists, with colors indicating the top 1% of artists for each season.", 140),
    caption = "Source: Wikipedia (via kkakey) · Source: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing.y = unit(2, "lines"),
    strip.text = element_text(color = pal[1], size = 10),
    plot.margin = margin(10, 20, 10, 30),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(margin = margin(5, 0, 15, 0))
  )
