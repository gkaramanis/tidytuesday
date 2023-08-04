library(tidyverse)
library(cowplot)
library(ggtext)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')

states_same <- states %>% 
  mutate(same_city = if_else(capital_city == largest_city, TRUE, FALSE))

col_names = c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature class", "feature code", "country code", "cc2", "admin1 code", "admin2 code", "admin3 code", "admin4 code", "population", "elevation", "dem", "timezone", "modification date")

cities5000 <- read_tsv("/Users/georgios/SynologyDrive/R/30daymapchallenge/2022/data/cities5000.txt", col_names = col_names)

cities_us <- cities5000 %>% 
  filter(`country code` == "US") %>% 
  select(`admin1 code`, name, longitude, latitude)

capitals <- states_same %>% 
  select(state, `admin1 code` = postal_abbreviation, name = capital_city, same_city) %>% 
  left_join(cities_us)
  
largest <- states_same %>% 
  select(state, `admin1 code` = postal_abbreviation, name = largest_city, same_city) %>% 
  left_join(cities_us)

all_cities <- bind_rows(capitals, largest) %>% 
  distinct()

cities_distance <- bind_cols(capitals %>% select(state, capital = name, lon1 = longitude, lat1 = latitude), largest %>% select(largest = name, lon2 = longitude, lat2 = latitude)) %>%
  rowwise() %>% 
  mutate(
    distance = geosphere::distHaversine(c(lon1, lat1), c(lon2, lat2)),
    x = lon1 + (lon1 - lon2)/2,
    y = min(c(lat1, lat2)) - 1,
    dist_km = if_else(distance != 0, paste(round(distance/1000), "km"), ""),
    dist_mi = if_else(distance != 0, paste(round(distance/1000 * 0.621371), "mi"), ""),
    dist_label = paste0(state, if_else(distance > 0, ", ", ""), dist_km),
    dist_label_mi = paste0(state, if_else(distance > 0, ", ", ""), dist_mi)
  )

us <- rgeoboundaries::gb_adm1("usa")

f1 <- "Outfit"
f2 <- "DIN Condensed"

p <- function(state_p) {
  ggplot() +
    geom_sf(data = us %>% filter(shapeName == state_p), fill = "#E4D9CE", color = "#463325") +
    # Distance
    geom_segment(data = cities_distance %>% filter(state == state_p), aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = "dashed") +
    # Points - Largest city
    geom_point(data = largest %>% filter(state == state_p), aes(longitude, latitude), size = 0.25) +
    geom_point(data = largest %>% filter(state == state_p), aes(longitude, latitude), size = 4, shape = 21) +
    # Points - Capital 
    geom_point(data = capitals %>% filter(state == state_p), aes(longitude, latitude), shape = "✪", size = 3, color = "#7B160E") +
    # City labels
    ggrepel::geom_text_repel(data = all_cities %>% filter(state == state_p), (aes(longitude, latitude, label = name, fontface = if_else(name %in% capitals$name, "bold", "plain"))), size = 3, family = f1, point.padding = 5, bg.color = "grey98", bg.r = 0.1, seed = 99) +
    # Plot stuff
    coord_sf(crs = "+proj=laea +lat_0=30 +lat_1=40 +lon_0=-100 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), clip = "off") +
    labs(
      title = cities_distance[cities_distance$state == state_p, ]$dist_label
      # Miles:
      # title = cities_distance[cities_distance$state == state_p, ]$dist_label_mi
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "plain", size = 12, hjust = 0.5, family = f2, color = "#000060")
    )
}

# Make list of all maps
maps <- map(.x = states$state, .f = p)

# Make grid of all maps with cowplot
gp <- plot_grid(plotlist = c(rep(NA, 3), maps), ncol = 8) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

# Plot the grid of all maps with cowplot
gpg <- ggdraw(gp) 

# Title plot
tp <- ggplot() +
  geom_richtext(aes(0, 0, label = "<span style='font-size:28px'>**Distance between the**</span><br><span style='color:#7B160E'>✪ </span>**Capital** & the 🞊Largest city<br><span style='font-size:28px'>**in each <span style='color:#000060'>state.</span>**</span>"), family = f1, size = 6, label.color = NA, hjust = 0, label.padding = unit(0.5, "line"), label.size = 1, fill = NA) +
  geom_tile(aes(-0.05, 0.05, height = 2.2, width = 0.12), fill = "#B5A99B") +
  geom_segment(aes(x = 1.9, xend = 3.56, y = 0.4, yend = 0.4), linetype = "dashed", linewidth = 0.75) +
  coord_cartesian(clip = "off", xlim = c(-0.1, 5), ylim = c(-1, 1)) + 
theme_void()

# Caption plot
cp <- ggplot() +
  geom_richtext(aes(1, 0, label = "Not in scale. The distances were calculated<br>using the haversine formula and rounded<br>**Source:** Wikipedia & GeoNames<br>**Graphic:** Georgios Karamanis"), family = f1, hjust = 1, size = 3.5, label.color = NA) +
  coord_cartesian(clip = "off", xlim = c(0, 1)) +
  theme_void()

# Add the title plot to the grid with patchwork (could've used cowplot for the inset but patchwork renders the geom_richtext() text correctly!)
gpg +
  inset_element(tp, left = 0, right = 0.4, bottom = 0.87, top = 0.98) +
  inset_element(cp, left = 0.5, right = 0.98, bottom = 0.02, top = 0.13) 
  
