library(tidyverse)
library(here)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_co2 <- food_consumption %>% 
  group_by(country) %>% 
  summarise(co2 = sum(co2_emmission)) %>% 
  top_n(., 10, co2) %>% 
  arrange(co2) %>% 
  mutate(n = -4:5) %>% 
  rowwise() %>%
  mutate(
    x = list(c(-10, 0, 0, -10)),
    y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))
  ) %>% 
  unnest(cols = c(x, y))
  
ggplot(food_co2) +
  geom_rect(aes(xmin = -42, ymin = n*4 - 1.4,
                xmax = -10, ymax = n*4 + 1.4), fill = "black", color = NA) +
  geom_polygon(aes(x, y, group = n), fill = "black", color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = co2/25, ymax = n*2 + 0.7), fill = "black", color = NA) +
  geom_text(aes(-40.5, n*4, label = country), family = "JetBrains Mono Bold", color = "white", hjust = 0, size = 8.5, check_overlap = TRUE) +
  geom_text(aes(co2/25-1, n*2, label = co2), family = "JetBrains Mono Medium", color = "white", hjust = 1, size = 4, check_overlap = TRUE) +
  annotate("text", 85, 17, label = "Total food carbon footprint\nKg CO2/person/year", family = "IBM Plex Sans Bold", color = "black", hjust = 1, size = 13, lineheight = 0.9) +
  annotate("text", 1, -9.5, label = "Source: nu3 | Graphic: Georgios Karamanis", family = "IBM Plex Sans", color = "black", hjust = 0, size = 4, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(0, 80, 20), labels = seq(0, 2000, 500)) +
  theme_minimal(base_family = "JetBrains Mono Medium") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "JetBrains Mono Medium", size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "black", size = 0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("2020-week08", "plots", "food-consumption.png"), dpi = 320, width = 12, height = 8)









