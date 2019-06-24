library(here)
library(tidyverse)
library(ggimage)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

city_count <- ufo_sightings %>% 
  group_by(city_area) %>% 
  summarise(total = n(), first_encounter = min(date_time)) %>% 
  top_n(n = 10, total) %>% 
  arrange(desc(total)) %>%
  mutate(year = as.numeric(substr(first_encounter, 5, 9)),
         city_area = str_to_title(city_area))

ggplot(city_count) +
  geom_segment(aes(x = city_area, y = year, 
                   xend = city_area, yend = 2019,
                   size = total),
               color = "orange") +
  # icon by https://www.iconfinder.com/korawan_m
  geom_image(aes(image = here("week-26", "img", "saucer.png"),
                 x = city_area, y = year - 4.5),
             asp = 1.5, size = 0.08, color = "purple") +
  geom_text(aes(label = year,
                x = city_area, y = year - 12),
            color = "grey30", family = "IBM Plex Mono Medium") +
  ylim(2019, 1930) +
  guides(size = guide_legend(title = "", nrow = 1)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey90",
                                   colour = "grey90"),
    panel.grid = element_blank(),
    text = element_text(family = "IBM Plex Sans Medium",
                        size = 14),
    legend.position = "top",
    axis.title = element_blank(),
    axis.text.y = element_blank()
  ) +

  ggsave(here("week-26", "ufo.png"))
