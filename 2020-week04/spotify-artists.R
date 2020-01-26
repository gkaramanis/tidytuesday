library(tidyverse)
library(here)
library(wesanderson)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify <- spotify_songs %>% 
  distinct(track_id, .keep_all = TRUE) %>% 
  distinct(tolower(track_name), tolower(track_artist), .keep_all = TRUE) %>% 
  mutate(year = as.numeric(str_sub(track_album_release_date, 1, 4))) %>%
  group_by(track_artist) %>%
  mutate(
    first_release = min(year),
    last_release = max(year),
    span = last_release - first_release
  ) %>%
  ungroup() %>% 
  mutate(track_artist = fct_reorder(track_artist, span))

spotify %>% 
  filter(span > 48) %>% 
  ggplot() +
  
  geom_segment(aes(x = first_release, y = track_artist,
                   xend = last_release, yend = track_artist), color = "pink", size = 2, alpha = 0.05) +
  
  geom_point(aes(year, track_artist), size = 8, color = "black") +
  geom_point(aes(year, track_artist, color = track_artist), size = 3) +
  geom_point(aes(year, track_artist), size = 0.01, color = "white") +

  geom_text(aes(first_release - 2, track_artist, label = paste0(span, " years")), hjust = 1, vjust = 2, family = "IBM Plex Sans Medium", size = 4, color = "#3bbcff", check_overlap = TRUE) +
  geom_text(aes(first_release - 2, track_artist, label = track_artist), hjust = 1, vjust = 0.4, family = "IBM Plex Sans Medium", size = 5, color = "white", check_overlap = TRUE) +
  
  geom_text(aes(first_release, track_artist, label = first_release), vjust = 3.5, family = "JetBrains Mono", size = 2.5, color = "#e69deb", check_overlap = TRUE) +
  geom_text(aes(last_release, track_artist, label = last_release), vjust = 3.5, family = "JetBrains Mono", size = 2.5, color = "#e69deb", check_overlap = TRUE) +
  
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 20), position = "top") +
  scale_colour_manual(values = wes_palette("Zissou1", 15, type = "continuous"), guide = FALSE) +
  
  labs(
    title = "Artists with the longer span of album releases on Spotify",
    caption = "Source: Spotify | Graphic: Georgios Karamanis"
  ) +
  
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "#640064", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "#e69deb", size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(color = "#500050", size = 0.3),
    panel.grid.minor.x = element_line(color = "#500050", size = 0.2),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(family = "IBM Plex Sans Medium", hjust = 0.5, size = 20, color = "white", margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "white", margin = margin(40, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 80)
  ) +
  
  ggsave(here::here("2020-week04", "plots", paste0("spotify-artists-", ".png")), dpi = 320, height = 10, width = 14)
  




