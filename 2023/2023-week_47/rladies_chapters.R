library(tidyverse)
library(gganimate)
library(magick)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 6, units = "in", dpi = 320)

# Read in TidyTuesday event data
rladies_chapters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

# Read in original data with coordinates
events_json <- jsonlite::fromJSON('https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json')

# Event id with coordinates only
events_coord <- events_json %>% 
  filter(type != "cancelled") %>% 
  select(id, lon, lat) %>% 
  mutate(id = as.double(id))

# Missing city coordinates in the original dataset
missing_cities <- tribble(
  ~chapter, ~m_lon, ~m_lat,
  "rladies-addis-ababa",	9.0300, 38.7591,
  "rladies-barranquilla",	11.2423, -74.7944,
  "rladies-cambridge",	52.2053, 0.1213,
  "rladies-comitan",	16.2269, -92.1133,
  "rladies-jujuy",	24.1850, 65.3133,
  "rladies-lexington-park",	38.2978, -76.4233,
  "rladies-milagro",	2.1428, -79.5575,
  "rladies-nairobi",	-1.2833, 36.8200,
  "rladies-ushuaia",	-54.8000, -68.3000,
  "rladies-villahermosa",	18.0500, -92.9500
)

# Create data frame with all cities
# Keep only in person meetups and calculate median coordinates for each city
# Theses are going to be used later for online events that have a default location somewhere in the Pacific Ocean
cities <- rladies_chapters %>% 
  left_join(events_coord) %>% 
  filter(location == "inperson") %>% 
  group_by(chapter) %>% 
  summarize(
    m_lon = median(lon, na.rm = TRUE),
    m_lat = median(lat, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  rows_update(missing_cities)

# Add coordinates to TidyTuesday dataset, calculate yearmonth for each event
# replace 
events_month <- rladies_chapters %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  left_join(events_coord) %>% 
  arrange(date)  %>% 
  mutate(
    f = row_number(),
    month = floor_date(as.Date(date), "months")
    ) %>% 
  left_join(cities) %>% 
  mutate(
    longitude = ifelse(lon == 179.1962 & location == "online", m_lon, lon),
    latitude = ifelse(lat == -8.5211 & location == "online", m_lat, lat)
  )

# World map
world <- map_data("world") %>%
  fortify()

# Fonts
f1 <- "Outfit"
f2 <- "SF Mono"

# Map animation
p1 <- ggplot(events_month) +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey88") +
  geom_point(aes(longitude, latitude, color = location, shape = location), size = 5, alpha = 0.8, stroke = 1.5) +
  scale_color_manual(values = c("#562457", "#b77ef0"), na.translate = FALSE) +
  scale_shape_manual(values = c(4, 6), na.translate = FALSE) +
  coord_fixed(expand = FALSE) +
  labs(
    title = "R-Ladies events",
    subtitle = "{format(frame_time, '%Y %B')}"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14)
  ) +
  transition_time(month)

# Cumulative number of in person and online events by month
events_lines <- events_month %>% 
  count(year, month, location) %>%
  group_by(location) %>%
  arrange(month) %>%
  mutate(cum_n = cumsum(n)) %>% 
  ungroup() %>% 
  filter(!is.na(location)) %>% 
  complete(month = seq.Date(min(month), max(month), by = "month"))

# Animation of line chart with cumulative number of events
p2 <- ggplot(events_lines) +
  geom_line(aes(month, cum_n, color = location), linewidth = 1) +
  scale_color_manual(values = c("#562457", "#b77ef0")) +
  scale_y_continuous(labels = scales:: number, position = "right", expand = expansion(mult = c(0.03, 0))) +
  scale_x_date(expand = expansion(mult = c(0, 0.01))) +
  labs(
    title = "Cumulative number of events"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(family = f1),
    axis.text.y = element_text(hjust = 0, family = f1),
    axis.title = element_blank(),
    panel.grid = element_line(linewidth = 0.15),
    plot.margin = margin(0, 20, 0, 20)
  ) +
  transition_reveal(month)

# Create the actual animations
g1 <- animate(p1, fps = 5, units = "in", width = 10, height = 5, res = 320)

g2 <- animate(p2, fps = 5, units = "in", width = 9.5, height = 2.5, res = 320)

# Read animations with magick
gif1 <- image_read(g1)
gif2 <- image_read(g2)

# Get the minimum number of frames between the two animations
gif_l <- pmin(nrow(image_info(gif1)), nrow(image_info(gif2)))

# Combine the two animations
final_gif <- image_append(c(gif1[1], gif2[1]), stack = TRUE)

for(i in 2:gif_l-1){
  combined <- image_append(c(gif1[i], gif2[i]), stack = TRUE)
  final_gif <- c(final_gif, combined)
}

# Write final animation
image_write_video(final_gif, framerate = 8, path = here::here("2023/2023-week_47/plots/rladies_chapters.mp4"))
