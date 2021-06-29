library(tidyverse)
library(sf)
library(rnaturalearth)
library(janitor)
library(ggmap)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

us <- c(left = -124.763068, bottom = 45.543541, right = -116.915989, top = 49.002494)
stamen <- get_stamenmap(us, zoom = 7, maptype = "terrain-background")

trails_shp <- read_sf(here::here("2020-week48", "data", "WA_RCO_Trails_2017-shp", "WA_RCO_Trails_2017.shp")) %>% 
  clean_names() %>% 
  select(name = tr_nm, geometry) %>% 
  st_transform(4326)

hike_shp <- hike_data %>% 
  left_join(trails_shp) %>% 
  filter(lengths(geometry) > 0)

ggmap(stamen) +
  geom_sf(data = hike_shp, aes(geometry =geometry), inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326)) +
  theme_void() 

ggsave(here::here("temp", paste0("washington-hiking-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
