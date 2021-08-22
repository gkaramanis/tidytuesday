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
    release_yearmonth = floor_date(as.Date(track_album_release_date), "months"),
    deci_year = decimal_date(release_yearmonth) - decade
  ) %>%
  group_by(release_yearmonth) %>%
  mutate(
    sp_med = median(speechiness),
    da_med = median(danceability)
    ) %>%
  ungroup()
 
spotify %>%
  na.omit() %>% 
  filter(release_year < 2020 & release_year > 1959) %>% 
 ggplot() +
  geom_line(aes(deci_year, sp_med, group = decade)) +
  geom_line(aes(deci_year, da_med, group = decade)) +
  facet_wrap(vars(decade)) +
  scale_y_continuous(limits = c(-0.1, 1)) +
  coord_polar() +
  theme_minimal() 
  
  ggsave(here::here("2020-week04", "plots", "temp", paste0("spotify-genres-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

