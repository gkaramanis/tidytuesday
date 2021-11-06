library(tidyverse)
library(camcorder)
library(spData)
library(sf)
library(ggfx)

gg_record(dir = "temp", device = "png", width = 9, height = 10, units = "in", dpi = 320)

data(world)

lat <- 30
lon <- 58
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

circle <- st_point(x = c(0, 0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = ortho) %>%
  st_transform(crs = 4326)

world_ortho <- st_cast(world, 'MULTIPOLYGON') %>%
  st_cast('POLYGON', do_split = TRUE) %>%
  st_transform(crs = ortho)

f1 = "Porpora"

ggplot(world_ortho) +
  with_blur(
    geom_sf(data = circle, fill = "cornflowerblue", color = NA, size = 2),
    sigma = unit(3, 'mm')
  ) +
  geom_sf(data = circle, fill = "#06216E", color = "black", size = 0.3) +
  geom_sf(aes(fill = lifeExp), size = 0.2, color = "#06216E") +
  scale_fill_fermenter(palette = "Purples", na.value = "#8c7d70") +
  labs(
    caption = "Data: spData package · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_colorbar(title = "Life Expectancy (Years)", title.position = "top", title.hjust = 0.5)) +
  theme_void(base_family = f1, base_size = 15) +
  theme(
    legend.position = "top",
    legend.title = element_text(color = "grey99", size = 22),
    legend.text = element_text(color = "grey97"),
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(3, "line"),
    plot.background = element_rect(fill = "#200034", color = NA),
    plot.caption = element_text(color = "grey97"),
    plot.margin = margin(20, 20, 20, 20)
  )
