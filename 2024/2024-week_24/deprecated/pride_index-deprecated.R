library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

pride_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')

pride_index_tags <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index_tags.csv')

pride_it <- pride_index %>% 
  left_join(pride_index_tags) %>% 
  # fix a typo
  mutate(campus_location = str_replace(campus_location, "Swarrthmore", "Swarthmore")) %>% 
  filter(rating == 5)

# campus_coord <- pride_it %>% 
#   distinct(campus_location) %>% 
#   tidygeocoder::geocode(address = campus_location, method = 'osm', lat = latitude , long = longitude)
# 
# dir.create(here::here("2024/2024-week_24/data/"))
# write_csv(campus_coord, here::here("2024/2024-week_24/data/campus_coord.csv"))

campus_coord <- read_csv(here::here("2024/2024-week_24/data/campus_coord.csv"))
  
pride_sf <- pride_it %>% 
  left_join(campus_coord) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326), remove = FALSE) %>% 
  mutate(side = if_else(longitude < -81.5, "left", "right")) %>% 
  group_by(side) %>% 
  arrange(latitude) %>% 
  mutate(
    campus_name = str_replace(campus_name, "University", "U"),
    x = if_else(side == "left", -125, -68),
    y = seq(25, 50, length.out = n()),
    hjust = if_else(side == "left", 1, 0)
    ) %>% 
  ungroup()

regions_divisions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv") %>% 
  janitor::clean_names() %>% 
  rename(abbr = state_code, full = state)

us_states <- usmap::us_map("states") %>% 
  left_join(regions_divisions) %>% 
  filter(!full %in% c("Alaska", "Hawaii"))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("Redon") %>% 
  colorspace::lighten(0.5)

ggplot(pride_sf) +
  geom_sf(data = us_states, aes(fill = region), color = "white") +
  geom_sf(aes(size = students), alpha = 0.3) +
  geom_text(aes(x, y, label = campus_name, hjust = hjust), family = f1b, size = 3) +
  ggbump::geom_sigmoid(aes(x = longitude, y = latitude, xend = x, yend = y, group = campus_name), linewidth = 0.2) +
  scale_fill_manual(values = pal) +
  scale_size_area(max_size = 10) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(0, 100, 0, 100)
  )
