library(tidyverse)
library(here)

# library(gtsummary)
# tbl_summary(hotels)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

countries_coord <- readr::read_csv("http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0") 
countries_ll <- countries_coord %>% 
  select(country_code = iso_a3, country = name, continent, lon = Longitude, lat = Latitude)

hotel_countries <- hotels %>% 
  select(hotel, country_code = country) %>% 
  left_join(., countries_ll) %>% 
  add_count(country) %>% 
  distinct(country, continent, lon, lat, n) %>% 
  arrange(n) %>%
  mutate(
    a = atan2(lon + 8.5, lat - 39.6),
    c = ifelse(lon < -8.5, -0.5, 0.5)
  )
  
# CN and other NA
# ggplot has iso_a3

worldmap <- borders("world", colour = "gray60", fill = "white", size = 0.05)

hotel_countries %>% 
  # filter(country != "Portugal") %>% 
  ggplot() +
  worldmap +
  geom_curve(data = subset(hotel_countries, lon < -8.502),
             aes(x = lon, y = lat,
                 xend = -8.5010436, yend = 39.5955067,
                 size = n, colour = continent), alpha = 0.25, curvature = -0.5) +
   geom_curve(data = subset(hotel_countries, lon > -8.502),
              aes(x = lon, y = lat,
                  xend = -8.5010436, yend = 39.5955067,
                  size = n, colour = continent), alpha = 0.25, curvature = 0.5) +              
  # geom_spoke(aes(x = lon, y = lat, angle = 3 * pi/2 - a, radius = 4, color = n), alpha = 0.7, arrow = arrow(length = unit(0.1,"cm"))) +
  scale_size_continuous(range = c(0.5, 4)) +
  # scale_alpha_continuous(range = c(0.1, 1)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) 

ggsave(here::here("2020-week07", "plots", "hotels-map.png"), dpi = 320)

