library(tidyverse)
library(osmdata)
library(here)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

streets <- sf_trees %>% 
  filter(latitude < 40 & latitude > 37.7) %>% 
  mutate(address = str_replace(address, "@", " ")) %>% 
  separate(address, sep = "^\\d+[Xx-]* *(- *\\d+ )*", c("number", "street"), remove = FALSE) %>% 
  add_count(street) %>% 
  filter(n > 1000)
  
san_francisco <- getbb("San Francisco") %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = san_francisco$osm_lines, color = "grey10", size = 0.1, alpha = 0.8) +
  geom_point(data = streets, aes(longitude, latitude, color = street, size = n), size = 0.15) +
  annotate("point", -122.4106, 37.7858, color = "#276DC2", size = 10) +
  annotate("text", -122.4106, 37.7858, color = "white", size = 2, label = "rstudio::conf", family ="JetBrains Mono") +
  scale_colour_viridis_d(guide = guide_legend(title = "Streets in San Francisco with\nthe most DPW-maintained trees", override.aes = list(size = 2))) +
  coord_sf(xlim = c(-122.51, -122.34), ylim = c(37.7, 37.81)) +
  labs(
    caption = "Source: DataSF | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "JetBrains Mono Bold") +
  theme(
    legend.title = element_text(size = 13, color = "grey10"),
    legend.text = element_text(size = 12, color = "grey70"),
    plot.background = element_rect(colour = NA, fill = "grey45"),
    plot.margin = margin(20, 40, 20, 80),
    plot.caption = element_text(family = "JetBrains Mono", margin = margin(20, 0, 0, 0))
  ) +
  ggsave(here::here("2020-week05", "plots", "sf-trees.png"), dpi = 320, width = 12.3, height = 7)

