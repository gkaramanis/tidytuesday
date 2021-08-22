library(tidyverse)
library(lubridate)

christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")


christmas_songs %>% 
  # filter(song == "ALL I WANT FOR CHRISTMAS IS YOU") %>% 
  mutate(date = mdy(weekid)) %>% 
  ggplot() +
  geom_line(aes(date, week_position, color = song, group = song)) +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
  

# ggsave(
#       here::here("week-52", "plots", "temp", paste0("christmas-songs-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320
#       )