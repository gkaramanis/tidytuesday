library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

holiday_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')

holiday_genres <- holiday_movies %>% 
  mutate(genres_list = as.list(str_split(genres, ","))) %>% 
  unnest(genres_list) %>% 
  mutate(decade = year %/% 10 * 10) %>% 
  group_by(decade) %>% 
  mutate(n_year = n_distinct(tconst)) %>% 
  ungroup() %>% 
  group_by(decade, genres_list) %>% 
  reframe(
    n = n(),
    pct = n / n_year * 100
    ) %>% 
  distinct() %>% 
  mutate(genres_list = fct_reorder(genres_list, -pct))

f1 <- "Outfit"
f2 <- "Asap Expanded"

highlight <- c("Comedy", "Drama", "Romance", "Family", "Animation")

p <- ggplot(holiday_genres, aes(decade, pct, fill = genres_list, label = genres_list)) +
  ggstream::geom_stream(aes(color = after_scale(colorspace::lighten(fill, 0.3))), type = "proportional", linewidth = 0.5) +
  ggstream::geom_stream_label(type = "proportional", color = "white", family = f1, aes(size = if_else(genres_list %in% highlight, 6, 3))) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_size_identity() +
  theme_void() +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text()
  )

p_labels <- ggplot(holiday_genres, aes(decade, pct, fill = genres_list, label = genres_list)) +
  ggstream::geom_stream_label(type = "proportional", color = "white", family = f1, aes(size = if_else(genres_list %in% highlight, 6, 3))) +
  theme_void()

labels_build <- ggplot_build(p_labels) %>% 
  .$data %>% 
  .[[1]] 


ggplot() +
  ggstream::geom_stream(data = holiday_genres, aes(decade, pct, fill = genres_list, color = after_scale(colorspace::lighten(fill, 0.3))), type = "proportional", linewidth = 0.5) +
  ggrepel::geom_text_repel(data = labels_build, 
                           aes(x = if_else(label == "Comedy", 1942, x), 
                               y = if_else(label == "Comedy", 0.95, y), 
                               label = label, 
                               color = if_else(label %in% highlight, "black", "grey20"),
                               size = if_else(label %in% highlight, 6, 3)), 
                           family = f2,
                           fontface = "bold",
                           seed = 999,
                           segment.size = 0.15
                           ) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_size_identity() +
  scale_color_identity() +
  scale_fill_manual(values = c("Family" = "#6fa5d2",
                               "Drama" = "#f0746b",
                               "Animation" = "#6edca3",
                               "Romance" = "#f4578d",
                               "Comedy" = "#f9c782"), na.value = "grey65") +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Unwrapping festive movie genres: Romance on the rise, animation in decline",
    subtitle = "Genre shares by decade from 1929 to 2023 for movies with the words holiday, Christmas, Hanukkah or Kwanzaa in their title. Movies can have multiple genres.",
    caption = "Source: IMDB · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(margin = margin(5, 0, 0, 0), size = 14),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = f2, face = "bold.italic", size = 18, color = "purple4"),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  )
