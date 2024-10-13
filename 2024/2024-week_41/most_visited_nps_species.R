library(tidyverse)
library(sf)
library(maps)
library(gt)
library(patchwork)
library(geomtextpath)
library(shadowtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11.5, height = 8, units = "in", dpi = 320)

most_visited_nps_species_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv') %>% 
  janitor::clean_names()

park_coords <- tribble(
  ~park_name, ~park_code, ~lat, ~lon,
  "Acadia National Park", "ACAD", 44.3386, -68.2733,
  "Bryce Canyon National Park", "BRCA", 37.5930, -112.1871,
  "Cuyahoga Valley National Park", "CUVA", 41.2808, -81.5678,
  "Glacier National Park", "GLAC", 48.7596, -113.7870,
  "Grand Canyon National Park", "GRCA", 36.0544, -112.1401,
  "Grand Teton National Park", "GRTE", 43.7904, -110.6818,
  "Great Smoky Mountains National Park", "GRSM", 35.6131, -83.5532,
  "Hot Springs National Park", "HOSP", 34.5217, -93.0424,
  "Indiana Dunes National Park", "INDU", 41.6533, -87.0524,
  "Joshua Tree National Park", "JOTR", 33.8734, -115.9010,
  "Olympic National Park", "OLYM", 47.8021, -123.6044,
  "Rocky Mountain National Park", "ROMO", 40.3428, -105.6836,
  "Yellowstone National Park", "YELL", 44.4280, -110.5885,
  "Yosemite National Park", "YOSE", 37.8651, -119.5383,
  "Zion National Park", "ZION", 37.2982, -113.0263
)

# Calculate total species count for each park
park_species_count <- most_visited_nps_species_data %>%
  count(park_code, name = "species_count")

# Create shared species count for all connections
shared_species <- most_visited_nps_species_data %>%
  select(park_code, sci_name) %>%
  # Self-join to find all park pairs sharing a species
  inner_join(., ., by = "sci_name") %>%
  # Keep only unique park pairs (avoid duplicates and self-pairs)
  filter(park_code.x != park_code.y) %>%
  # Count shared species for each park pair
  count(park_code.x, park_code.y, name = "shared_species") %>%
  # Ensure each park appears as both first and second in pairs
  bind_rows(., rename(., park_code.x = park_code.y, park_code.y = park_code.x)) %>%
  # Sort by number of shared species, descending
  arrange(desc(shared_species)) %>%
  # Keep only the strongest connection for each park pair
  distinct(park_code.x, park_code.y, .keep_all = TRUE) %>%
  # Keep only the top 50 connections
  slice_head(n = 50)

# Join all data together
plot_data <- shared_species %>%
  # Add coordinates for first park in pair
  left_join(park_coords, by = c("park_code.x" = "park_code")) %>%
  # Add coordinates for second park in pair
  left_join(park_coords, by = c("park_code.y" = "park_code"), suffix = c(".x", ".y")) %>%
  # Add species count for first park
  left_join(park_species_count, by = c("park_code.x" = "park_code")) %>%
  rename(species_count.x = species_count) %>%
  # Add species count for second park
  left_join(park_species_count, by = c("park_code.y" = "park_code"), suffix = c(".x", ".y"))

# Prepare US map data
us_map <- st_as_sf(maps::map("usa", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Convert park coordinates to sf object and transform to Albers projection
park_locations <- st_as_sf(plot_data, coords = c("lon.x", "lat.x"), crs = 4326) %>%
  st_transform(crs = st_crs(us_map))

# Extract transformed coordinates for first park in each pair
park_coords_transformed <- st_coordinates(park_locations)
plot_data$x.x <- park_coords_transformed[, 1]
plot_data$y.x <- park_coords_transformed[, 2]

# Transform and extract coordinates for second park in each pair
park_locations_y <- st_as_sf(plot_data, coords = c("lon.y", "lat.y"), crs = 4326) %>%
  st_transform(crs = st_crs(us_map))
park_coords_transformed_y <- st_coordinates(park_locations_y)
plot_data$x.y <- park_coords_transformed_y[, 1]
plot_data$y.y <- park_coords_transformed_y[, 2]

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

t <- park_coords %>% 
  select(park_code, park_name) %>% 
  gt() %>% 
  tab_options(column_labels.hidden = TRUE) %>% 
  tab_options(
    table.font.names = f1b,
    table.font.size = 10,
    data_row.padding = 5,
    table_body.hlines.width = 0
    )

col1 <- "#00203F"
col2 <- "#ADEFD1"

p <- ggplot() +
  geom_sf(data = us_map, fill = "lightgray", color = NA) +
  geom_sf(data = park_locations, aes(size = species_count.x), alpha = 0.8, fill = col2, color = col1, stat = "unique", shape = 21) +
  geom_textcurve(data = plot_data %>% filter(shared_species >= 700), aes(x = x.x, y = y.x, xend = x.y, yend = y.y, linewidth = shared_species, label = scales::number(shared_species)), curvature = 0.35, lineend = "round", family = f1b, alpha = 0.6, show.legend = FALSE, color = col1) +
  geom_curve(data = plot_data %>% filter(shared_species < 700), aes(x = x.x, y = y.x, xend = x.y, yend = y.y, linewidth = shared_species), curvature = 0.35, lineend = "round", alpha = 0.6, show.legend = FALSE, color = col1) +
  geom_shadowtext(data = park_locations %>% filter(!park_code.x %in% c("ZION", "GRTE")), aes(label = park_code.x, geometry = geometry), nudge_y = 100000, color = "white", family = f1b, stat = "sf_coordinates", size = 5, bg.color = "gray30") +
  geom_shadowtext(data = park_locations %>% filter(park_code.x %in% c("ZION", "GRTE")), aes(label = park_code.x, geometry = geometry), nudge_y = 50000, nudge_x = -140000, color = "white", family = f1b, stat = "sf_coordinates", size = 5, bg.color = "gray30") +
  scale_size_continuous(range = c(5, 20)) +
  scale_linewidth_continuous(range = c(0.1, 4)) +
  MetBrewer::scale_color_met_c("Hokusai1", direction = -1) +
  labs(title = "Top biodiversity connections between National Parks",
       subtitle = str_wrap("Each node represents a park, sized by its total species count. Curved lines show shared species between park pairs, with thickness indicating the number of common species.", 90),
       caption = "Source: NPSpecies, IRMA · Graphic: Georgios Karamanis",
       size = "Species count"
       ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "gray99", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
    )

p + 
  inset_element(t, left = 0.81, bottom = 0, right = 1, top = 0.365, align_to = "full") &
  theme(plot.background = element_rect(fill = "gray99", color = NA)) +
  theme(legend.position = c(0.1, 0.12),
        legend.title.position = "top",
        legend.title = element_text(face = "bold", family = f1b),
        legend.direction = "horizontal")

