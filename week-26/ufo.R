library(here)
library(tidyverse)
library(ggimage)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

city_count <- ufo_sightings %>% 
  group_by(city_area) %>% 
  summarise(total = n(), first_encounter = min(date_time)) %>% 
  top_n(n = 10, total) %>% 
  mutate(year = as.numeric(substr(first_encounter, 5, 9)),
         city_area = str_to_title(city_area),
         city_area = fct_reorder(city_area, -total))
  

ggplot(city_count) +
  geom_segment(aes(x = city_area, y = year, 
                   xend = city_area, yend = 2019,
                   size = total),
               color = "orange1") +
  # icon by https://www.iconfinder.com/korawan_m
  geom_image(aes(image = here("week-26", "img", "saucer.png"),
                 x = city_area, y = year - 2.2),
             asp = 1.2, size = 0.05, color = "purple3") +
  geom_text(aes(label = year,
                x = city_area, y = year - 8),
            color = "grey30", family = "IBM Plex Mono") +
  geom_text(aes(label = total,
                x = city_area, y = 2017.7),
            nudge_x = 0.3,
            color = "orange1", family = "IBM Plex Mono Bold") +
  ylim(2019, 1930) +
  labs(title = "Top 10 cities with the most reported UFO encounters") +
  # guides(size = guide_legend(nrow = 1, title = "total encounters")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#e0e7f3",
                                   colour = "grey90"),
    panel.grid = element_blank(),
    legend.position = "",
    text = element_text(family = "IBM Plex Sans Medium",
                              size = 14),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(margin = margin(-22, 0, 0, 0),
                               family = "IBM Plex Sans Medium",
                               size = 10, hjust = 0.1),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(margin = margin(0, 0, 2, 0))
  ) +

  ggsave(here("week-26", "ufo.png"), width = 10, height = 8)
