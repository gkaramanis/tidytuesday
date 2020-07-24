library(tidyverse)
library(futurevisions)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astro_eva <- astronauts %>% 
  # distinct(name, total_eva_hrs, .keep_all = TRUE) %>% 
  filter(eva_hrs_mission > 0)

top_eva <- astro_eva %>% 
  filter(eva_hrs_mission > 33) %>%
  rowwise() %>% 
  mutate(
    r = runif(1, -5, 5),
    x = list(seq(0, eva_hrs_mission, length.out = 150)),
    y = list(year_of_mission + 0.7 * sin(3 * (120 %/% eva_hrs_mission) / eva_hrs_mission * pi * x)) # Returns bigger period for longer missions, modulo makes it binned and integer 
  ) %>% 
  unnest(c(x, y))

pal_top <- futurevisions("jupiter")

decade <- data.frame(x = 0,
                     y = seq(1965, 2015, by = 10)
                     ) 
pal_dec <- futurevisions("europa")

ggplot(astro_eva) +
  geom_tile(data = decade, aes(x = x, y = y, fill = y, width = Inf), colour = NA) +
  geom_text(data = decade, aes(x = 109, y = y, label = paste0("'", str_sub(y - 5, -2))), hjust = 1, family = "DIN Condensed Bold", size = 5) +
  geom_point(aes(eva_hrs_mission, year_of_mission), size = 0.5, colour = "grey10") +
  geom_path(data = top_eva, aes(x, y, group = id, colour = mission_title)) +
  geom_point(data = top_eva, aes(eva_hrs_mission, year_of_mission, colour = mission_title)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_y_reverse() +
  scale_color_manual(values = pal_top) +
  scale_fill_gradient(low = pal_dec[3], high = pal_dec[4]) +
  coord_polar(start = -pi/2) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("2020-week29", "plots", "temp", paste0("astronauts-eva", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

