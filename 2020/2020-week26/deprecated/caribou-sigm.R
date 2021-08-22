library(tidyverse)
library(ggbump)
library(rnaturalearth)

locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')

canada <- ne_countries(country = "Canada", returnclass = "sf") %>% 
  select(name, geometry)

movement <- locations %>% 
  separate(timestamp, into = c("date", "time"), sep = " ") %>% 
  group_by(animal_id, date) %>% 
  mutate(
    longitude = median(longitude),
    latitude = median(latitude)
  ) %>% 
  ungroup() %>% 
  distinct(animal_id, season, date, longitude, latitude) %>%
  group_by(animal_id) %>% 
  arrange(date) %>%
  mutate(
    # p2_lon = lead(longitude),
    # p2_lat = lead(latitude),
    # a = atan2((p2_lat - latitude), (p2_lon - longitude)),
    # d = sqrt((p2_lat - latitude) ^ 2 + (p2_lon - longitude) ^ 2),
    # d_sum = sum(d, na.rm = TRUE),
    last_longitude = last(longitude),
    last_latitude = last(latitude)
  ) %>%
  ungroup() 

movement_sigm <- movement %>% 
  group_by(animal_id) %>% 
  mutate(
    min_date = as.Date(min(date)),
    max_date = as.Date(max(date))
  ) %>% 
  ungroup() %>% 
  distinct(animal_id, min_date, max_date, last_longitude, last_latitude) %>%
  left_join(individuals) %>% 
  mutate(
    deploy_off_longitude = coalesce(deploy_off_longitude, last_longitude),
    deploy_off_latitude = coalesce(deploy_off_latitude, last_latitude),
    deploy_off_type_x = -122.7 + 0.33 * as.numeric(factor(deploy_off_type))
    )

ggplot(movement_sigm) +
  geom_sf(data = canada, aes(geometry = geometry), fill = "#f2f2f2", colour = "grey70", size = 0) +
  # geom_path(data = movement, aes(longitude, latitude, group = animal_id), colour = "grey70", size = 0.1) +
  # geom_point(aes(deploy_on_longitude, deploy_on_latitude)) +
  geom_sigmoid(aes(x = deploy_off_longitude, y = deploy_off_latitude, xend = deploy_off_type_x, yend = 52.3, group = animal_id, colour = deploy_off_type), direction = "y", alpha = 0.2, size = 0.4) +
  geom_point(aes(deploy_off_longitude, deploy_off_latitude), size = 0.8) +
  geom_point(aes(x = deploy_off_type_x, y = 52.3), colour = "pink") +
  # geom_text(aes(x = -116, y = deploy_off_type_y + 0.01, label = deploy_off_type)) +
  scale_color_viridis_d() +
  coord_sf(xlim = c(-126.8, -115.5), ylim = c(52, 57.5), expand = FALSE) +
  theme_void() 

ggsave(here::here("2020-week26", "plots", "temp", paste0("caribou-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
