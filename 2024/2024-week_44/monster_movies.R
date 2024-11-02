library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

monster_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')

monsters <- monster_movies %>% 
  select(year, genres, average_rating) %>% 
  mutate(genres = str_split(genres, ",")) %>% 
  unnest(genres) %>% 
  group_by(year, genres) %>% 
  summarise(
    n_movies = n(),
    median_rating = median(average_rating)
    ) %>% 
  ungroup()

monster_cloud <- monster_movies %>% 
  select(year, primary_title) %>% 
  unnest_tokens(word, primary_title) %>% 
  anti_join(stop_words) %>% 
  filter(str_detect(tolower(word), "monster", negate = TRUE)) %>%
  filter(str_detect(tolower(word), "movie", negate = TRUE)) %>%
  count(word)

f1 <- "Graphik"
f1b <- "Graphik Compact"

pumpkin <- magick::image_read(here::here("2024/2024-week_44/pumpkin.png"))

set.seed(99)

ggplot(monster_cloud) +
  geom_text_wordcloud_area(aes(label = word, size = n, color = n), mask = pumpkin, family = f1b) +
  scale_size_area(max_size = 13.5) +
  MetBrewer::scale_color_met_c("OKeeffe2", direction = -1) +
  labs(
    title = "Horror's favorite words",
    subtitle = "Most frequent words in monster movie titles (1919-2024), excluding stop (common) words, 'monster' and 'movie'",
    caption = "Source: IMDb · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#0A0A0A", color = NA),
    plot.title = element_text(color = "#E6E6FA", size = 20, face = "bold"),
    plot.subtitle = element_text(color = "#E6E6FA"),
    plot.caption = element_text(color = "#E6E6FA"),
    plot.margin = margin(10, 10, 10, 10)
  )