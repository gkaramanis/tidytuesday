library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9.5, units = "in", dpi = 320)

polling_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv')

states <- data.frame(
  state = state.abb,
  state_name = state.name
)

pp <- polling_places %>% 
  count(election_date, state, county_name, location_type) %>% 
  filter(is.na(location_type)) %>% 
  left_join(states) %>% 
  mutate(
    state_lower = tolower(state_name),
    county_lower = tolower(county_name)
  )

counties <- map_data("county") %>% 
  rename(state_lower = region, county_lower = subregion) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 
  
counties_sf <- counties %>% 
  group_by(group) %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

counties_pp <- counties_sf %>% 
  left_join(counties %>% distinct(group, state_lower, county_lower), by = "group") %>% 
  left_join(pp) %>% 
  filter(!is.na(election_date))

states_sf <- map_data("state") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(group) %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

f1 <- "Outfit"

ggplot() +
  geom_sf(data = states_sf, color = NA) +
  geom_sf(data = counties_pp, aes(fill = n), linewidth = 0.05, color = "white") +
  geom_sf(data = states_sf, linewidth = 0.3, color = "white", fill = NA) +
  colorspace::scale_fill_binned_sequential("BurgYl", guide = guide_colorsteps(show.limits = TRUE, title.position = "top")) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")) +
  facet_wrap(vars(election_date), ncol = 2, strip.position = "bottom", labeller = function(x) {format(x, "%d %B %Y")}) +
  labs(
    caption = "Source: The Center for Public Integrity · Graphic: Georgios Karamanis",
    fill = "Number of polling places\nwith missing 'type of polling location'"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.76, 0.17),
    legend.direction = "horizontal",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title = element_text(margin = margin(0, 0, 3, 0), size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
