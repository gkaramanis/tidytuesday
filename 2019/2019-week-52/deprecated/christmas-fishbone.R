library(tidyverse)
library(here)
library(viridis)

x_songs <- read_rds(here("week-52", "data", "christmas-songs.rds")) %>% 
  select(title = title_x, everything()) %>% 
  mutate(
    title = str_to_title(title),
    year =  as.numeric(str_sub(week_id, -4))
  ) %>% 
  select(title, release_year, year, week_position) %>% 
  # filter(week_position < 25) %>% 
  group_by(title) %>% 
  mutate(song_nr = group_indices()) %>%
  ungroup() %>%
  group_by(title, year) %>%
  mutate(max_position = max(week_position)) %>%
  ungroup()

spine <- x_songs %>% 
  group_by(title) %>% 
  mutate(max_year = max(year)) %>% 
  distinct(title, release_year, max_year, song_nr)


# as.numeric(x_songs$year != x_songs$release_year + 1)

ggplot(x_songs) +
  geom_vline(xintercept = 2019, color = "grey70", linetype = "dashed") +
  # "Spine" from release year to last Billboard year
  geom_segment(data = subset(spine, release_year != max_year), aes(x = release_year, xend = max_year, y = song_nr, yend = song_nr), color = "grey50") +
  # Billboard years
  geom_segment(aes(x = year, y = song_nr, xend = year, yend = song_nr + year %% 2 * - 0.6 + 0.3)) +
  # geom_segment(aes(x = year, y = song_nr, xend = year, yend = song_nr + (1 - max_position/100) * 0.9, color = max_position), size = 3) +
  # geom_point(aes(x = release_year, y = song_nr) +
  # geom_point(aes(x = year, y = song_nr)) +
  geom_point(aes(x = year, y = song_nr + year %% 2 * - 0.6 + 0.3, color = max_position)) +
  scale_x_continuous(breaks = seq(1930, 2020, 10), minor_breaks = 1930:2020) +
  scale_y_continuous(breaks = 1:53) +
  # scale_colour_gradient(low = "red", high = "purple") +
  # scale_color_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) + 
  scale_color_viridis(direction = -1, option="plasma") +
  guides(colour = guide_colorbar(reverse = TRUE)) +
  theme_minimal(base_family = "IBM Plex Sans") 

ggsave(
    here::here("week-52", "plots", "temp", paste0("christmas-fishbone-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 16, width = 12
  )

  
  