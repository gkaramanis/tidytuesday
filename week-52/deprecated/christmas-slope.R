library(tidyverse)
library(here)

x_songs <- read_rds(here("week-52", "data", "christmas-songs.rds")) %>% 
  select(title = title_x, everything()) %>% 
  mutate(
    title = str_to_title(title),
    year =  as.numeric(str_sub(week_id, -4))
    ) %>% 
  distinct(title, year, release_year) %>%
  mutate(title = fct_reorder(title, year, .desc = TRUE))
  
ggplot(x_songs) +
  # slope 1
  # geom_segment(aes(x = 1, xend = 2, y = release_year, yend = year)) +
  # slope 2
  # geom_segment(aes(y = title, yend = title, x = release_year, xend = year)) +
  # curves
    geom_curve(data = subset(x_songs, year != release_year), aes(y = 0, yend = 0, x = release_year, xend = year)) +
  ggsave(
    here::here("week-52", "plots", "temp", paste0("christmas-slope-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320
  )
