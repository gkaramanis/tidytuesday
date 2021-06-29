library(tidyverse)
library(here)
library(sf)
library(ggimage)
library(shadowtext)
library(ggforce)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

central_park <- read_sf(dsn = here::here("week-44", "data", "central-park", "CentralPark.shp"), layer = "CentralPark")

sq <- nyc_squirrels %>% 
  mutate(
    icon = case_when(
      str_detect(other_activities, "dead") ~ "tombstone"
      # str_detect(other_activities, "dog") ~ "dog",
      # str_detect(other_activities, "canoodling") ~ "balloon",
      # str_detect(other_activities, "upside down") ~ "sloth"
    )
  ) %>% 
  filter(!is.na(icon))

ggplot(sq) +
  geom_sf(data = central_park, color = "black") +
  geom_image(
    aes(long, lat,
        image = paste0(here::here("week-44", "data", "icons"), "/", icon, ".png")),
    size = 0.04, asp = 1) +
  geom_shadowtext(aes(long , lat - 0.0015, label = unique_squirrel_id),
            color = "grey80", family = "IBM Plex Sans Medium", size = 3, hjust = 0.9) +
  geom_label(data = data.frame(),
             aes(-73.948, 40.771,
                 label = "IN LOVING MEMORY\nOF\n\n39G-AM-1018-01\nAND\n21F-AM-1018-03\n\nYOU ARE FOREVER\nIN OUR HEARTS"),
             family = "IBM Plex Serif Medium",
             label.padding = unit(0.75, "lines"),
             label.size = 0,
             label.r = unit(0.45, "lines"),
             alpha = 0.7) +
  labs(caption = "Source: NYC Squirrel Census | Graphic: Georgios Karamanis") +
  scale_x_continuous(limits = c(-73.99, -73.94)) +
  scale_y_continuous(limits = c(40.7644, 40.8022)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "darkslateblue", colour = NA),
    plot.background = element_rect(fill = NA, color = "darkslateblue"),
    plot.margin = margin(10, 10, 0, 10),
    plot.caption = element_text(family = "IBM Plex Sans", size = 7, color = "darkslateblue", vjust = 2.5)
  ) 

ggsave(
    here::here("week-44", "plots", "temp", paste0("nyc-squirrels-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7, width = 7
    )

