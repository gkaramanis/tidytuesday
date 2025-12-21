library(tidyverse)
library(sf)
library(crosstalk)
library(leaflet)
library(reactable)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv') |> 
  rename(long = lat, lat = long)

# Individual roundabouts for mapping
cities_ra_sf <- roundabouts_clean |> 
  filter(country == "Sweden") |> 
  add_count(town_city) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# City-level summary for table
cities_ra <- roundabouts_clean |> 
  filter(country == "Sweden") |> 
  count(town_city, county_area, state_region, country)

# SharedData for interactive linking
cities_ra_map <- cities_ra_sf |> 
  SharedData$new(group = "roundabouts")

cities_ra_data <- cities_ra |> 
  SharedData$new(group = "roundabouts")

cities_ra_map_leaflet <- leaflet(cities_ra_map) |> 
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(radius = 0.3,
                   fillColor = "orange",
                   fillOpacity = 0.7,
                   stroke = FALSE
  )

cities_ra_table <- reactable(
  cities_ra_data,
  selection = "multiple",
  onClick = "select",
  rowStyle = list(cursor = "pointer"),
  minRows = 10,
  searchable = TRUE
)

htmltools::browsable(
  htmltools::tagList(cities_ra_map_leaflet, cities_ra_table)
)