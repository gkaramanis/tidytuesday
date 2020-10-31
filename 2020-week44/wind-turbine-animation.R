library(tidyverse)
library(gganimate)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')


w_turb <- wind_turbine %>% 
  group_by(province_territory) %>% 
  mutate(
    rotor_r = rotor_diameter_m / 2,
    province_n = cur_group_id(),
    n = n()
  ) %>%
  select(province_n, n, longitude, latitude, hub_height_m, rotor_r) %>% 
  summarise(across(.cols = c(province_n, n, longitude, latitude, hub_height_m, rotor_r), median)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    blade1 = round(runif(1, 0, 2 * pi), 1),
    blade2 = round(blade1 + 0.67 * pi, 1),
    blade3 = round(blade2 + 0.67 * pi, 1),
    t = list(seq(0, 2 * pi, 0.1))
  ) %>% 
  unnest(t)


anim <- ggplot(w_turb) +
  geom_segment(aes(x = province_n * 200, y = 0, xend = province_n * 200, yend = hub_height_m), stat = "unique") +
  geom_spoke(aes(x = province_n * 200, y = hub_height_m, angle = log(n) * t + blade1, radius = rotor_r), stat = "unique") +
  geom_spoke(aes(x = province_n * 200, y = hub_height_m, angle = log(n) * t + blade2, radius = rotor_r), stat = "unique") +
  geom_spoke(aes(x = province_n * 200, y = hub_height_m, angle = log(n) * t + blade3, radius = rotor_r), stat = "unique") +
  geom_text(aes(x = province_n * 200, y = hub_height_m + 90, label = n), stat = "unique", family = "Atkinson Hyperlegible", size = 8) +
  geom_text(aes(x = province_n * 200, y = -20, label = str_wrap(province_territory, 10)), stat = "unique", vjust = 1, lineheight = 0.9, family = "Atkinson Hyperlegible Bold", size = 6) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme() +
  transition_states(t)

anim_save(animation = anim, filename = here::here("2020-week44", "plots", "wind_turbine_animation.gif"), renderer = gifski_renderer(), width = 1536, height = 512)
