library(tidyverse)
library(lubridate)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 15, height = 12, units = "in", dpi = 320)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

audio_features <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

billb_feat <- billboard %>% 
  left_join(audio_features) %>% 
  distinct(week_id, song, song_id, week_position, spotify_genre) %>% 
  mutate(
    week_id = mdy(week_id),
    year = year(week_id),
    spotify_genre = str_remove_all(spotify_genre, "\\[|\\]|'|\""),
    spotify_genre = str_split(spotify_genre, ", ")
    ) %>% 
  filter(spotify_genre != "" & !is.na(spotify_genre)) %>% 
  unnest(spotify_genre) %>% 
  group_by(year, spotify_genre) %>% 
  count(year, spotify_genre)

f1 = "Fira Sans Compressed"
f2 = "Futura"

ggplot(billb_feat, aes(x = year, y = n, fill = spotify_genre)) +
  geom_bar(position = "fill", stat = "identity", width = 1) +
  geom_text(aes(label = spotify_genre, size = n), position = position_fill(vjust = 0.5), check_overlap = TRUE, family = f1, color = "grey10") +
  scale_fill_viridis_d(option = "turbo", begin = 0.05, end = 0.95) +
  scale_size_continuous(range = c(0.4, 5)) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Music genres of the Billboard Hot 100",
    subtitle = "Proportion of all song genres by year according to Spotify",
    caption = "Source: Data.World, Billboard.com and Spotify · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA),
    axis.text.x = element_text(family = f2, size = 20, face = "bold", margin = margin(10, 0, 0, 0), color = "grey97"),
    plot.margin = margin(20, 5, 20, 5),
    plot.title = element_text(family = f2, face = "bold", size = 38, color = "grey97"),
    plot.subtitle = element_text(family = f1, size = 24, margin = margin(5, 0, 10, 0), color = "grey97"),
    plot.caption = element_text(family = f1, size = 12, margin = margin(10, 0, 0, 0), color = "grey97")
  )

# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" makingof.mp4

# ggsave(here::here("temp", paste0("billboard-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

