library(tidyverse)
library(here)
library(osmdata)


tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

philadelphia <- getbb("Philadelphia USA")%>%
  opq()%>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

fire_h <- tickets %>% 
  filter(lat > 39.9 & lat < 40.1) %>%
  filter(lon > -75.26 & lon < -75.01) %>% 
  filter(violation_desc == "FIRE HYDRANT")

ggplot() +
  geom_sf(data = philadelphia$osm_lines, color = "grey5", size = 0.4, alpha = 0.8) +
  geom_point(data = fire_h, aes(lon, lat), color = "firebrick1", size = 0.1, alpha = 0.5) +
  coord_sf(xlim = c(-75, -75.27), 
           ylim = c(39.9, 40.1),
           expand = FALSE) +
  labs(
    title = "Fire Hydrant Parking Tickets in Philadelphia, 2017",
    caption = "Source: Open Data Philly | Graphic: Georgios Karamanis"
    ) +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 24, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(margin = margin(5, 0, 0, 0)),
    panel.border = element_rect(colour = "black", fill= NA, size= 0.5)
  ) +
ggsave(
      here::here("week-49", "plots", "temp", paste0("tickets-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 10
      )
 
