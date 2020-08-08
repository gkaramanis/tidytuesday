library(tidyverse)
library(rnaturalearth)
library(sf)
library(colorspace)

country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv') %>% 
  mutate(country_name = case_when(
    country == "EL" ~ "Greece",
    country == "UK" ~ "United Kingdom",
    TRUE ~ country_name
  )) %>% 
  filter(type == "Imports" | type == "Exports") %>% 
  pivot_longer(cols = "2016":"2018", names_to = "year", values_to = "value") %>% 
  pivot_wider(names_from = "type", values_from = "value") 

totals_2018 <- country_totals %>%
  filter(year == 2018) %>% 
  pivot_longer(cols = 'Imports':'Exports', names_to = "type", values_to = "value") %>% 
  group_by(type) %>% 
  mutate(
    total_eu = sum(value),
    share = value / total_eu * 100
    )


# Get countries in dataset
energy_countries <- country_totals %>% 
  distinct(country_name) %>% 
  pull()

# Read in centroids
centroids <- read_tsv("2020-week17/data/country-centroids.csv")

# Get sf objects, filter by countries in dataset
countries_sf <- ne_countries(scale = 50, returnclass = "sf") %>% 
  filter(region_un != "Americas" & region_un != "Oceania") %>% 
  left_join(centroids) %>% 
  select(country_name = name, geometry, latitude, longitude, pop_est) %>% 
  mutate(country_name = case_when(
    country_name == "Macedonia" ~ "North Macedonia",
    country_name == "Czech Rep." ~ "Czechia",
    country_name == "Bosnia and Herz." ~ "Bosnia & Herzegovina",
    country_name == "Republic of Serbia" ~ "Serbia",
    TRUE ~ country_name
  )) %>% 
  mutate(type = list(c("Imports", "Exports"))) %>% 
  unnest(type) %>% 
  left_join(totals_2018) %>% 
  mutate(value_pc = value / pop_est)


# energy_map <- country_totals %>% 
#   left_join(countries_sf)

# https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/
disp_win <- st_sfc(st_point(c(-15, 27)), st_point(c(85, 60)), crs = 4326)
disp_win_trans <- st_transform(disp_win, crs = 3035)
disp_win_coord <- st_coordinates(disp_win_trans)

ggplot(countries_sf) +
  geom_sf(aes(geometry = geometry, fill = share), colour = "grey10", size = 0.1) +
  geom_text(aes(x = longitude, y = latitude, label = country)) +
  scale_fill_gradient(low = darken("#343854", 0.3), high = darken("#F0C742", 0.1), na.value = "grey10") +
  coord_sf(crs = st_crs(3035), xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y']) +
  facet_wrap(vars(type)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#09283C"),
    strip.text = element_text(colour = "grey95"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020-week32", "plots", "temp", paste0("european-energy-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 8)

