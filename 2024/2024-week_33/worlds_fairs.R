library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("tidytuesday-temp/"), width = 10, height = 6, dpi = 320)

worlds_fairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv')

# Run once to geocode the cities
# wf_coord1 <- worlds_fairs %>% 
#   count(city, country, sort = TRUE) %>% 
#   tidygeocoder::geocode(city = city, country = country, method = 'osm', lat = latitude , long = longitude)
#   
# wf_coord2 <- wf_coord1 %>% 
#   filter(is.na(longitude)) %>% 
#   select(city, country) %>% 
#   tidygeocoder::geocode(city = city, method = 'osm', lat = latitude , long = longitude)
#   
# wf_coord1 %>% 
#   filter(!is.na(longitude)) %>% 
#   bind_rows(wf_coord2) %>% 
#   write_csv(here::here("2024/2024-week_33/data/cities.csv"))

proj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- read_sf("/Users/georgios/Documents/R/30daymapchallenge/2022/data/world.geo.json") %>% 
  st_transform(proj, ylim = c(-55, 80), xlim = c(-180, 180))

wf_coord <- read_csv(here::here("2024/2024-week_33/data/cities.csv"))

wf <- worlds_fairs %>% 
  add_count(city, country) %>% 
  group_by(city, country) %>% 
  summarise(
    label = list(paste(start_year, name_of_exposition, collapse = "\n"))
  ) %>% 
  ungroup() %>% 
  left_join(wf_coord) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
  st_transform(proj, ylim = c(-55, 80), xlim = c(-180, 180))

cities_highlight <- tribble(
  ~city, ~country,
  "London", "United Kingdom",
  "Paris", "France",
  "Chicago", "United States",
  "Osaka", "Japan",
  "Philadelphia", "United States",
  "New York City", "United States",
  "Montreal", "Canada",
  "Brussels", "Belgium",
  "Shanghai", "China",
  "New York", "United States",
  "Dubai", "United Arab Emirates"
)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

# Map
ggplot() +
  geom_sf(data = world, fill = "white", color = "#C2C2C2", linewidth = 0.15) +
  geom_sf(data = wf, aes(size = n), shape = 21, alpha = 0.5, fill = "#93EDE4", stroke = 0.4, color = "darkblue") +
  scale_size_continuous(range = c(1, 4)) +
  ggnewscale::new_scale(new_aes = "size") +
ggrepel::geom_text_repel(data = wf %>% filter(city %in% cities_highlight$city), aes(geometry = geometry, label = city, size = n), stat = "sf_coordinates", family = f1b, segment.size = 0.3, min.segment.length = 0.25, point.padding = 1, max.overlaps = 11, seed = 199, bg.color = alpha("white", 0.3)) +
  scale_size_continuous(range = c(3, 5)) +
  coord_sf(expand = FALSE, xlim = c(-150, 175), default_crs = 4326) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#E4E6E6", color = NA)
  )
    
# Most visitors bar chart
worlds_fairs |> 
  slice_max(order_by = visitors, n = 5) |> 
  mutate(name_of_exposition = fct_reorder(name_of_exposition, visitors)) |> 
  ggplot(aes(x = visitors, y = name_of_exposition, label = paste0(name_of_exposition, " (", start_year, ")"))) +
  geom_col(aes(fill = if_else(city == "Shanghai", "#93EDE4", "#C2C2C2")), width = 0.7) +
  geom_text(aes(x = 1), hjust = 0, family = f1b, size = 9, fontface = "bold") +
  geom_text(aes(label = paste(visitors, "mil.")), hjust = 1, nudge_x = -1, family = f1b, size = 9) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = f1) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = NA, color = NA)
    )

# Participating countries bar chart
worlds_fairs |> 
  mutate(i = row_number()) |> 
  ggplot() +
  # geom_col(aes(x = i, y = attending_countries, fill = if_else(start_year %in% c(1851, 2010, 2022), "#E493ED", "#C2C2C2"))) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 20)
  )
