library(tidyverse)
library(sf)
library(rnaturalearth)
library(janitor)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

trails_shp <- read_sf(here::here("2020-week48", "data", "WA_RCO_Trails_2017-shp", "WA_RCO_Trails_2017.shp")) %>% 
  clean_names() %>% 
  select(name = tr_nm, geometry)

wa <- ne_states(iso_a2 = "US", returnclass = "sf") %>% 
  filter(name == "Washington")

hike_shp <- hike_data %>% 
  left_join(trails_shp) %>% 
  filter(lengths(geometry) > 0)

ggplot(hike_shp) +
  geom_sf(data = wa) +
  geom_sf(aes(geometry = geometry), size = 0.15) +
  theme_void() +
  ggsave(here::here("temp", paste0("washington-hiking-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

