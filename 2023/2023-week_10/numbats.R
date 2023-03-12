library(tidyverse)
library(sf)
library(patchwork)
library(ggpointdensity)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv') %>% 
  mutate(nightday = if_else(between(hour, 6, 18), "day", "night"))

aus <- read_sf("https://raw.githubusercontent.com/tonywr71/GeoJson-Data/master/australian-states.json")

aus_fac <- bind_rows(aus, aus) %>% 
  mutate(nightday = rep(c("day", "night"), each = 8))

# numbats_sf <- numbats %>% 
#   filter(!is.na(decimalLongitude)) %>% 
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>% 
#   st_set_crs(4326) %>% 
#   st_transform(st_crs(aus)) %>% 
#   mutate(nightday = if_else(between(hour, 6, 18), "day", "night"))
# 
# night_hotspots <- sfhotspot::hotspot_gistar(numbats_sf %>% filter(nightday == "night"), cell_size = 0.5, kde = FALSE) %>% 
#   mutate(nightday = "night")
# 
# day_hotspots <- sfhotspot::hotspot_gistar(numbats_sf %>% filter(nightday == "day"), cell_size = 0.5, kde = FALSE) %>% 
#   mutate(nightday = "day")
# 
# hotspots <- bind_rows(day_hotspots, night_hotspots)

hours <- numbats %>% 
  mutate(nightday = if_else(between(hour, 6, 18), "day", "night")) %>% 
  count(nightday, hour) %>% 
  filter(!is.na(nightday))

f1 <- "DIN Condensed"
f2 <- "Futura"

coln <- "#9B4A97"
cold <- "#F9A12E"
colbg <- "#E6ECF6"

h <- ggplot(hours) +
  geom_col(aes(hour, n, fill = nightday), width = 0.6) +
  geom_text(aes(hour, n + 50, label = n, color = nightday), family = f2, size = 3) +
  scale_x_continuous(breaks = 0:23, limits = c(-0.5, 23.5)) +
  scale_color_manual(values = c(cold, coln)) +
  scale_fill_manual(values = c(cold, coln)) +
  labs(
    title = "Numbat sightings",
    subtitle = "By <span style='color:dodgerblue4'>**hour of day**</span>, and <span style='color:#F9A12E'>**day**</span> or <span style='color:#9B4A97'>**night**</span>"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = colbg, color = NA),
    axis.text.x = element_text(color = "dodgerblue4"),
    plot.margin = margin(0, 0, 20, 0),
    plot.title = element_text(size = 22, family = f2, face = "bold"),
    plot.subtitle = element_markdown(size = 14, family = f2)
  )

m <- ggplot() +
  geom_sf(data = aus_fac, aes(fill = nightday), color = "black") +
  scale_fill_manual(values = c(cold, coln)) +
  guides(fill = "none") +
  ggnewscale::new_scale_fill() +
  geom_pointdensity(data = numbats %>% filter(!is.na(nightday)), aes(decimalLongitude, decimalLatitude), shape = 4, size = 3) +
  MetBrewer::scale_color_met_c("VanGogh3") +
  coord_sf(clip = "off") +
  facet_wrap(vars(nightday), strip.position = "bottom") +
  labs(caption = "Source: Atlas of Living Australia · Graphic: Georgios Karamanis") +
  guides(color = guide_colorsteps(title.position = "top", title = "Total number of neighbouring sightings")) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2.5, "lines"),
    legend.title = element_text(family = f2, hjust = 0.5, size = 10),
    strip.text = element_blank(),
    plot.background = element_rect(fill = colbg, color = NA),
    plot.caption = element_text(hjust = 0.5, color = "dodgerblue4")
  )

  
h / m +
  plot_layout(heights = c(1, 4)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = colbg, color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

