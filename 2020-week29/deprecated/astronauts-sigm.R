library(tidyverse)
library(ggbump)

library(gggibbous)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

missions <- astronauts %>% 
  distinct(year_of_mission, name, ascend_shuttle, descend_shuttle) %>% 
  mutate(decade = year_of_mission %/% 10 * 10) %>% 
  group_by(name) %>% 
  mutate(
    mission_n = cur_group_id()
  ) %>% 
  ungroup() %>% 
  group_by(ascend_shuttle) %>% 
  mutate(
    asc_n = cur_group_id()
  ) %>%
  ungroup() %>% 
  group_by(descend_shuttle) %>% 
  mutate(
    desc_n = cur_group_id()
  )
  
ggplot(missions) +
  geom_text(aes(-2, asc_n, label = ascend_shuttle), size = 1, hjust = 1) +
  geom_text(aes(0, mission_n, label = name), size = 1) +
  geom_text(aes(2, desc_n, label = descend_shuttle), size = 1, hjust = 0) +
  geom_sigmoid(aes(x = -1.9, xend = -0.1, y = asc_n, yend = mission_n, group = interaction(asc_n, mission_n))) +
  geom_sigmoid(aes(x = 0.1, xend = 1.9, y = mission_n, yend = desc_n, group = interaction(desc_n, mission_n))) +
	coord_polar() +
  theme_void() +
  facet_wrap(vars(decade)) +
  ggsave(here::here("2020-week29", "plots", "temp", paste0("astronauts-sigm", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
