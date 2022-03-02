library(tidyverse)
library(camcorder)
library(sf)
library(ggrepel)

gg_record(dir = "temp", device = "png", width = 15, height = 10, units = "in", dpi = 320)

# Read in stations csv
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') %>%
  janitor::clean_names()

# Read in stations shapefile and rename fuel types
# Alternative_Fueling_Stations.zip in 2022/2022-week_09/data needs to be extracted first! 
stations_shp <- read_sf(here::here("2022/2022-week_09/data/Alternative_Fueling_Stations/Alternative_Fueling_Stations.shp")) %>% 
  janitor::clean_names() %>% 
  mutate(fuel_type = case_when(
    fuel_type == "CNG" | fuel_type == "LNG" ~ "Natural Gas",
    fuel_type == "BD" ~ "Biodiesel",
    fuel_type == "ELEC" ~ "Electric",
    fuel_type == "E85" ~ "Ethanol",
    fuel_type == "HY" ~ "Hydrogen",
    fuel_type == "LPG" ~ "Propane",
    TRUE ~ fuel_type
    )
    )

# Filter cities with most stations (using csv dataset for speed, then join stations shapefile) 
cities <- stations %>%
  filter(fuel_type_code == "ELEC") %>% 
  count(state, city, sort = TRUE) %>%
  head(15) %>% 
  left_join(stations_shp) %>% 
  select(city, state, geometry) %>% 
  group_by(city) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  st_as_sf()

# Read in primary roads shapefile and simplify
roads <- read_sf(here::here("2022/2022-week_09/data/tl_2017_us_primaryroads/tl_2017_us_primaryroads.shp")) %>% 
  rmapshaper::ms_simplify() %>% 
  janitor::clean_names()

# Calculate length of roads and filter the 20 longest
longest_roads <- roads %>% 
  group_by(fullname) %>% 
  summarise(road_length = sum(st_length(geometry))) %>% 
  ungroup() %>% 
  slice_max(order_by = road_length, n = 20) %>% 
  mutate(fullname = str_replace(fullname, "I- ", "I-"))

# Read in US boundary and simplify
us <- rgeoboundaries::gb_adm0("USA") %>% 
  rmapshaper::ms_simplify()

# Bounding box for cropping the map later
# Modified from https://github.com/tidyverse/ggplot2/issues/2090
bb <- st_sfc(
  st_polygon(list(cbind(
    c(-119, -74, -74, -119, -119), 
    c(22, 22, 51, 51, 22) 
  ))), crs = "WGS84") %>% 
  st_transform(crs = st_crs("ESRI:102003")) # USA Contiguous Albers Equal Area Conic, also used for plotting the map

# Fonts
f1 <- "Newsreader Text"

# Register medium weight as family
systemfonts::register_font(
  name = "Newsreader Text Medium",
  plain = "/Users/georgios/Library/Fonts/NewsreaderText-Medium.ttf"
)

f1m <- "Newsreader Text Medium"
f2 <- "Outfit"

# Plot
ggplot() +
  # US boundary
  geom_sf(data = us, size = 0.25, fill = "#FBF9F9", color = "#cbc5c2") +
  # All roads
  geom_sf(data = roads, size = 0.2, color = "#e0dcda") +
  # Longest roads
  geom_sf(data = longest_roads, size = 0.35, color = "#e0dcda") +
  # Stations
  geom_sf(data = stations_shp %>% filter(fuel_type == "Electric"), size = 0.01, alpha = 0.5, color = "cornflowerblue") +
  # Longest roads labels
  geom_text_repel(data = longest_roads, aes(label = fullname, geometry = geometry), stat = "sf_coordinates", color = "grey60", family = f1m, bg.color = "grey99", seed = 1999) +
  # Cities
  geom_text_repel(data = cities, aes(label = city, geometry = geometry), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5, bg.color = "grey99", segment.size = 0.2, seed = 1999) +
  # Crop map with bounding box and reproject
  coord_sf(xlim = c(st_bbox(bb)["xmin"], st_bbox(bb)["xmax"]), 
           ylim = c(st_bbox(bb)["ymin"], st_bbox(bb)["ymax"]),
           crs = st_crs("ESRI:102003"),
           expand = FALSE) +
  labs(
    title = "EV charging stations in the contiguous United States",
    subtitle = "Showing the top 15 cities with the most stations",
    caption = "Source: U.S. Department of Transportation · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(hjust = 0, size = 18, family = f2, face = "bold", margin = margin(15, 0, 0, 0)),
    plot.subtitle = element_text(hjust = 0, size = 14, family = f2),
    plot.caption = element_text(hjust = 1, size = 10, color = "grey50", family = f2, margin = margin(0, 0, 10, 0))
  )
