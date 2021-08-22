library(tidyverse)
library(here)
library(lubridate)
library(tidylog)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify <- spotify_songs %>% 
  distinct(track_id, .keep_all = TRUE) %>% 
  distinct(tolower(track_name), tolower(track_artist), .keep_all = TRUE) %>% 
  mutate(
    track_album_release_date = as.Date(track_album_release_date),
    release_year = as.numeric(str_sub(track_album_release_date, 1, 4)),
    decade = release_year %/% 10 * 10,
    # release_yearmonth = floor_date(as.Date(track_album_release_date), "months"),
    loudness = loudness + 60
  ) %>%
  pivot_longer(danceability:valence, names_to = "variable") %>% 
  filter(
    variable == "acousticness" |
    variable == "liveness" | 
    variable == "speechiness"
    ) %>%
    mutate(variable = toupper(variable))

ggplot(spotify) +
  geom_segment(aes(x = track_album_release_date, xend = track_album_release_date, y = -value/2, yend = value/2), alpha = 0.1, color = "#D0E0FB") +
  scale_x_date(expand = c(0.002, 0.002), breaks = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), date_labels = "%Y") +
  facet_wrap(vars(variable), scales = "free_y", ncol = 1) +
  labs(
    title = toupper("Evolution of acousticness, liveness, and speechiness\nof Spotify songs through the years"),
    subtitle = "",
    caption = "Source: Spotify | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Mono") +
  theme(
    strip.text = element_text(color = "grey80"),
    panel.background = element_rect(fill = "#243046", color = NA),
    plot.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#D0E0FB"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, color = "grey90", margin = margin(0, 0, 20, 0), hjust = 0.5),
    plot.caption = element_text(color = "grey50", margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("2020-week04", "plots", "temp", paste0("spotify-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 8
)

