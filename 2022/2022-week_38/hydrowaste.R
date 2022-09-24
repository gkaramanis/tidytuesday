library(tidyverse)
library(camcorder)
library(sf)
library(rmapshaper)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 6.5, units = "in", dpi = 320)

# Read in data
hydrowaste <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv') %>% 
  janitor::clean_names()

# Mediterranean coastline shapefile
medit <- read_sf(here::here("2022/2022-week_38/data/iho/iho.shp")) %>% 
  ms_simplify(keep = 0.01, method = "dp") %>%
  ms_dissolve()

# Set buffer to filter plants, 0.1 degrees = 11.1 km 
buff <- st_buffer(medit, 0.1)

hydro_medit <- hydrowaste %>% 
  # keep plants within 10km of water (sea or big lake)
  filter(coast_10km == 1) %>% 
  # convert to sf
  st_as_sf(coords = c("lon_wwtp", "lat_wwtp")) %>% 
  st_set_crs(4326) %>% 
  # keep plants within 11.1 km of the sea
  st_intersection(buff)

# Get capital cities of Mediterranean countries
capitals <- read_csv("https://gist.githubusercontent.com/ofou/df09a6834a8421b4f376c875194915c9/raw/355eb56e164ddc3cd1a9467c524422cb674e71a9/country-capital-lat-long-population.csv") %>% 
  janitor::clean_names() %>% 
  filter(country %in% c("Albania", "Bosnia and Herzegovina", "Gibraltar", "Algeria", "Morocco", "Croatia", "Montenegro", "Cyprus", "Libya", "Lebanon", "Egypt", "Malta", "Monaco", "Slovenia", "Syria", "Tunisia", "Turkey", "Greece", "Spain", "Israel", "Italy")) %>% 
  mutate(capital_city = str_replace(capital_city, " \\(", "\n\\("))

f1 <- "Outfit"

ggplot() +
  geom_sf(data = medit, size = 0.1, fill = "black", color = "grey30") +
  geom_sf(data = hydro_medit, aes(size = pop_served, fill = factor(qual_pop), color = after_scale(colorspace::lighten(fill, 0.6))), stroke = 0.1, shape = 21) +
  geom_point(data = capitals, aes(x = longitude, y = latitude), color = "grey80", size = 0.1, alpha = 0.6) +
  geom_text(data = capitals, aes(x = longitude, y = latitude, label = capital_city), color = "grey80", family = f1, size = 2, alpha = 0.6, lineheight = 0.9, hjust = 0, nudge_x = 0.2) +
  scale_size_continuous(range = c(0.2, 7), name = "Population served", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_manual(labels = c("Reported", "Estimated"), values = c("#FCF050A0", "#EA337EA0"), name = NULL) +
  labs(
    title = "Wastewater treatment plants of the Mediterranean coast",
    subtitle = str_wrap("Wastewater treatment plants with an estimated outfall location within 10 km of the sea. The size of the circle corresponds to the population served. The color shows if the population is reported as ‘population equivalent’ by a national or regional dataset (yellow) or estimated without wastewater discharge available (purple).", 135),
    caption = "Source: Macedo et al, 2022, Distribution and characteristics of wastewater\ntreatment plants within the global river network · Graphic: Georgios Karamanis"
  ) +
  guides(
    size = guide_legend(nrow = 1, order = 1, override.aes = list(fill = "grey65", color = "gray75")),
    fill = guide_legend(nrow = 1, override.aes = list(size = 4))
    ) +
  coord_sf(expand = FALSE, clip = "off") +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey22", color = NA),
    legend.text = element_text(color = "grey50", margin = margin(0, 5, 0, 0), hjust = 0),
    legend.title = element_text(color = "grey95"),
    legend.position = c(0.22, 0.02),
    plot.title = element_text(size = 18, color = "gray95"),
    plot.subtitle = element_text(color = "gray85", margin = margin(5, 0, 30, 0)),
    plot.caption = element_text(color = "gray75", margin = margin(25, 0, 0, 0), size = 7),
    plot.margin = margin(0, 25, 5, 20)
  )
  
