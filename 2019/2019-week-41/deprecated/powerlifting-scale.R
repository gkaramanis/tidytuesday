library(tidyverse)
library(here)
library(gganimate)

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

anim <-
ggplot(ipf_lifts) +
  geom_segment(aes(x = bodyweight_kg, xend = bodyweight_kg, y = 1, yend = 2)) +
  coord_polar() +
  transition_time(date)

animate(anim, nframes = 100, fps = 20)
  
  anim_save(here::here("week-41", "figures", "temp", paste0("powerlifting-scale-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gif")))
