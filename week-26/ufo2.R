library(here)
library(tidyverse)
library(ggimage)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

city_count <- ufo_sightings %>%
  select(city_area, date_time) %>%
  mutate(year = as.numeric(str_sub(date_time, -10, -7)),
         city_area = str_to_title(city_area)) %>%
  add_count(city_area) %>% 
  filter(n>230)

ggplot() +
  geom_tile(data = city_count, aes(x = year, y = city_area,
                height = 0.5, width = 0.3),
            alpha = 0.2, fill = "orange1") +
  scale_x_continuous(breaks = seq(1940, 2010, by = 10)) +
  labs(
    title = "Top 10 cities in the world with\nthe most reported UFO encounters",
    caption = "Source: NUFORC | Graphics: Georgios Karamanis"
    ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#e0e7f3",
                                   colour = "#e0e7f3"),
    panel.grid = element_blank(),
    legend.position = "",
    axis.title = element_blank(),
    axis.text = element_text(family = "IBM Plex Mono", size = 14),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(family = "IBM Plex Sans", size = 20),
    plot.subtitle = element_text(family = "IBM Plex Sans", size = 14),
    plot.caption = element_text(family = "IBM Plex Sans",
                                margin = margin(40, 0, 0, 0),
                                color = "grey60", size = 14)
  ) +
  
  ggsave(here("week-26", "ufo2.png"), width = 13, height = 8)

