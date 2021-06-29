library(here)
library(tidyverse)
library(ggimage)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

city_count <- ufo_sightings %>%
  mutate(every_year = str_sub(date_time, -10, -7)) %>% 
  group_by(city_area) %>% 
  summarise(total = n(), first_encounter = min(every_year)) %>% 
  top_n(n = 10, total) %>% 
  mutate(year = as.numeric(first_encounter),
         city_area = str_to_title(city_area),
         city_area = fct_reorder(city_area, year))
  

ggplot(city_count) +
  geom_segment(aes(x = city_area, y = year, 
                   xend = city_area, yend = 1970,
                   size = total),
               color = "orange1") +
  # icon by https://www.iconfinder.com/korawan_m
  geom_image(aes(image = here("week-26", "img", "saucer.png"),
                 x = city_area, y = year - 0.8),
             asp = 1.4, size = 0.05, color = "purple3") +
  geom_text(aes(label = year,
                x = city_area, y = year - 3),
            color = "grey40", family = "IBM Plex Mono Bold",
            size = 5, alpha = 0.6) +
  geom_text(aes(label = total,
                x = city_area, y = 1967.5),
            nudge_x = 0.37, size = 8, alpha = 0.7,
            color = "orange1", family = "IBM Plex Mono Bold") +
  # "legend"
  geom_text(aes(label = "year of first reported\nencounter at the city",
                x = 3, y = 1941),
            color = "grey60", family = "IBM Plex Mono Bold",
            size = 5, alpha = 0.9, hjust = 0) +
  geom_segment(aes(x = 2.8, y = 1941,
                   xend = 1.4, yend = 1941),
               color = "grey60", alpha = 0.5) +
  geom_text(aes(label = "total number of encounters\nduring all years",
                x = 8, y = 1950),
            color = "orange1", family = "IBM Plex Mono Bold",
            size = 5, alpha = 0.2, hjust = 0) +
  geom_segment(aes(x = 7.8, y = 1950,
                   xend = 6.4, yend = 1955),
               color = "orange1", alpha = 0.2) +
  scale_y_reverse(position = "right") +
  coord_cartesian(ylim = c(1940, 1967)) +
  geom_text(aes(label = "Top 10 cities in the world with\nthe most reported UFO encounters",
                x = 11, y = 1941),
            family = "IBM Plex Sans Bold",
            size = 8, hjust = 1) +
  labs(
    # title = "Top 10 cities\nin the world with\nthe most reported\nUFO encounters",
    caption = "Source: NUFORC | Graphics: Georgios Karamanis") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#e0e7f3",
                                   colour = "#e0e7f3"),
    panel.grid = element_blank(),
    legend.position = "",
    text = element_text(family = "IBM Plex Sans Bold",
                              size = 14),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "IBM Plex Sans Bold", 
                               size = 14, hjust = 0.2, color = "purple3"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # plot.title = element_text(hjust = 1, size = 30,
    #                           margin = margin(0, 0, 0, -100)),
    plot.subtitle = element_text(family = "IBM Plex Sans"),
    plot.caption = element_text(margin = margin(40, 0, 0, 0),
                                color = "grey60")
  ) 

ggsave(here("week-26", "ufo.png"), width = 13, height = 8)
