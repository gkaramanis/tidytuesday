library(tidyverse)
library(gstat)
library(sf)
library(marmap)
library(ggnewscale)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7.8, units = "in", dpi = 320)

cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')

# Code for interpolation adapted from
# https://rspatial.org/analysis/4-interpolation.html

# Keep non-NA values and rename coordinates to x, y
cities_nona <- cities %>% 
  filter(!is.na(wind)) %>% 
  mutate(x = lon, y = lat)

# Get a raster file to be used as a grid for the interpolation and as the "background" of the map
us <- getNOAA.bathy(
  resolution = 6,
  lon1 = min(cities_nona$lon),
  lon2 = max(cities_nona$lon) + 6,
  lat1 = min(cities_nona$lat),
  lat2 = max(cities_nona$lat)
  )

# Create the grid
us_grid <- us %>% 
  marmap::as.raster()

# The actual interpolation, I don't really understand everything here :)
gs <- gstat(formula = wind ~ 1, locations = ~ x + y, data = cities_nona)
v <- variogram(gs, width = 1)
fvs <- fit.variogram(v, vgm("Sph"))

k <- gstat(formula = wind ~ 1, locations= ~ x + y, data = cities_nona, model = fvs)
kp <- terra::interpolate(us_grid, k)

# Read in US shapefile, used to mask the interpolation results
us_sf <- sf::st_read(here::here("2022/2022-week_51/data/USA_2_GADM_fips/USA_2_GADM_fips.shp")) %>% 
  rmapshaper::ms_dissolve()

# Mask the result, reproject and convert to data frame
kp_us <- terra::mask(kp, us_sf) %>% 
  terra::rast(crs = st_crs(us_sf)) %>% 
  terra::project("EPSG:5070") %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(value = 3) %>% 
  filter(!is.na(value))

# Reproject the "grid" raster, convert to data frame and keep only bathymetric data
# to be used as the "background" of the map
us_bg <- us %>% 
  marmap::as.raster() %>% 
  terra::rast() %>% 
  terra::project("EPSG:5070") %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(value = 3) %>% 
  filter(value <= 0)

# Read in world coastlines
coast <- read_sf(here::here("2022/2022-week_51/data/ne_10m_coastline/ne_10m_coastline.shp"))

# Convert data frame to sf, to show cities with recorded values
cities_nona_sf <- cities_nona %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Create US states sf
us_states_sf <- us_sf %>% 
  rmapshaper::ms_dissolve(field = "NAME_1")

# Draw the map
f1 <- "Outfit"

ggplot() +
  geom_raster(data = us_bg, aes(x, y, fill = value)) +
  scale_fill_distiller(palette = "BuPu", direction = -1, guide = "none") +
  new_scale_fill() +
  geom_raster(data = kp_us, aes(x, y, fill = value)) +
  geom_sf(data = coast, linewidth = 0.5, color = "grey80", fill = NA) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  geom_sf(data = us_states_sf, linewdith = 0.1, fill = NA, color = "#ffffff60") +
  geom_sf(data = cities_nona_sf, size = 1, shape = 4, color = "midnightblue") +
  guides(fill = guide_colorbar(title.position = "top")) +
  coord_sf(crs = 5070, xlim = c(min(kp_us$x), max(kp_us$x)), ylim = c(min(kp_us$y), max(kp_us$y))) +
  labs(
    title = "Kriging of mean wind speed",
    subtitle = "Calculated from recorded values from 217 cities (indicated with x)",
    caption = 'Source: "Collection and Analysis of Weather Forecast Error Data"\nby Sai Shreyas Bhavanasi, Harrison Lanier, Lauren Schmiedeler, and Clayton Strauch\nGraphic: Georgios Karamanis',
    fill = "Wind speed (m/s)"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.82, 1.08),
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.title = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(5, 0, 15, 0), size = 14),
    plot.caption = element_text(color = "grey30"),
    plot.margin = margin(0, 10, 0, 10)
  )
  



