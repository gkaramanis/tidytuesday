library(tidyverse)
library(rnaturalearth)
library(sf)

country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv') %>% 
  mutate(country_name = case_when(
    country == "EL" ~ "Greece",
    country == "UK" ~ "United Kingdom",
    TRUE ~ country_name
  ))

# Get countries in dataset
energy_countries <- country_totals %>% 
  distinct(country_name) %>% 
  pull()

# Read in centroids
centroids <- read_tsv("2020-week17/data/country-centroids.csv")

# Get sf objects, filter by countries in dataset
countries_sf <- ne_countries(country = c(energy_countries, "Macedonia", "Czech Republic", "Bosnia and Herzegovina", "Republic of Serbia"), scale = 50, returnclass = "sf") %>% 
  left_join(centroids) %>% 
  select(country_name = name, geometry, latitude, longitude) %>% 
  mutate(country_name = case_when(
    country_name == "Macedonia" ~ "North Macedonia",
    country_name == "Czech Rep." ~ "Czechia",
    country_name == "Bosnia and Herz." ~ "Bosnia & Herzegovina",
    country_name == "Republic of Serbia" ~ "Serbia",
    TRUE ~ country_name
  ))


energy_map <- country_totals %>% 
  left_join(countries_sf)

ggplot(energy_map) +
  geom_sf(aes(geometry = geometry, fill = country_name)) +
  geom_text(aes(x = longitude, y = latitude, label = country_name)) +
  coord_sf(crs = st_crs(3035)) +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("2020-week32", "plots", "temp", paste0("european-energy-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
