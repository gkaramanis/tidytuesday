library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

mta_art <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/mta_art.csv')

station_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/station_lines.csv')

# https://github.com/CityOfNewYork/nyc-geo-metadata/tree/main
sub_lines_sf <- read_sf(here::here("2025/2025-week_30/data/Subway_view_5083976324868604013.geojson")) |> 
  janitor::clean_names()

sub_stations_sf <- read_sf(here::here("2025/2025-week_30/data/SubwayStation_view_-2956341149621885519.geojson")) |> 
  janitor::clean_names()

# https://catalog.data.gov/dataset/mta-subway-stations-and-complexes
sub_stations <- read_sf(here::here("2025/2025-week_30/data/MTA_Subway_Stations_and_Complexes.csv")) |> 
  janitor::clean_names()

sub_stations_long <- sub_stations |> 
  separate_longer_delim(station_i_ds, delim = "; ") |> 
  mutate(station_id = as.numeric(station_i_ds))

sub_stations_sf_art <- sub_stations_sf |> 
  left_join(sub_stations_long) |> 
  left_join(mta_art, by = c("stop_name" = "station_name")) 

lines_selection <- sub_stations_sf_art |> 
  count(line.x, sort = TRUE) |> 
  filter(!is.na(line.x)) |> 
  slice_max(n = 8, order_by = n) |>
  pull(line.x)

ggplot(sub_lines_sf |> filter(line %in% lines_selection)) +
  geom_sf() +
  geom_sf(data = sub_stations_sf |> filter(line %in% lines_selection), size = 0.2) +
  ggrepel::geom_text_repel(data = sub_stations_sf_art |> filter(line.x %in% lines_selection), aes(label = art_title, geometry = geometry), stat = "sf_coordinates", size = 2) +
  facet_wrap(vars(line), ncol = 4)

