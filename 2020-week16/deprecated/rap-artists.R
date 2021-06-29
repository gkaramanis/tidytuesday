library(tidyverse)
library(janitor)
library(ggimage)
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
  group_by(year) %>% 
  add_count(name = "year_total") %>% 
  mutate(
    year_n = row_number(),
  ) %>% 
  ungroup()

ggplot(timeline) +
  geom_image(aes(x = year + 0.47, y = -year_n * 2 - 0.5, image = img), size = 0.019, asp = 1) +
  geom_rect(aes(xmin = year, ymin = -year_n * 2, xmax = year + 0.99, ymax = -year_n * 2 - 1, alpha = 1/critic_rating), fill = "white") +
  geom_textbox(aes(x = year, y = -year_n * 2, label = title_artist), fill = NA, hjust = 0, vjust = 1, size = 2, family = "IBM Plex Sans", width = unit(55, "pt"), height = unit(100, "pt"), box.padding = unit(c(60, 5, 0, 5), "pt")) +
  # geom_point(aes(x = year, y = -year_n * 2)) +
  scale_alpha(range = c(0.05, 0.95)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F7F7F7", colour = NA)
  ) 

ggsave(here::here("2020-week16", "plots", "temp", paste0("rap-artists-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 40, height = 40)
