library(tidyverse)
library(camcorder)
library(ggrepel)
library(rgeoboundaries)
library(patchwork)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')

swe_7 <- sevens %>% 
  filter(winner == "Sweden" | loser == "Sweden")

# Run this once to save a CSV

# library(tidygeocoder)
# cities <- unique(swe_7$venue, swe_15$venue)
# cities_geocode <- tibble(city = cities) %>% 
#   geocode(city = city)
# write_csv(cities_geocode, file = here::here("2022/2022-week_21/data/cities_geocode.csv"))

# Then read in CSV
cities_geocode <- read_csv(here::here("2022/2022-week_21/data/cities_geocode.csv")) %>% 
  distinct()

swe_7_wl <- swe_7 %>% 
  left_join(cities_geocode, by = c("venue" = "city")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, venue) %>% 
  mutate(
    result = if_else(winner == "Sweden", "W", "L"),
    res_list = paste(result, collapse = ""),
    res_label = paste0(year, ": ", res_list),
    tourn_w = if_else(tournament == "European Trophy" & year == 2016, TRUE, FALSE)
    ) %>% 
  ungroup() %>% 
  distinct(year, venue, res_list, res_label, tourn_w, long, lat) %>% 
  group_by(venue) %>% 
  mutate(label = paste0(venue, "\n", paste(res_label, collapse = "\n"))) %>%
  ungroup() %>% 
  select(-year, -res_list, -res_label) %>% 
  distinct()

# Read in world shapefile and simplify
world_raw <- rgeoboundaries::gb_adm0()
world <- rmapshaper::ms_simplify(world_raw, keep = 0.1)

# Create tournament list and plot
tournaments <- swe_7 %>% 
  distinct(year = lubridate::year(date), tournament, venue) %>% 
  mutate(i = row_number())

t <- ggplot(tournaments) +
  geom_text(data = tournaments, aes(0, -i, label = paste(tournament, venue, year, sep = "·")), hjust = 1, size = 2.7, family = f1, color = "#784108") +
  coord_fixed(xlim = c(-10, 0), clip = "off") +
  theme_void() 

# Fonts
f1 <- "Victor Mono"
f2 <- "Publico Headline"
f2b <- "Publico Headline Black"

# Main plot
p <- ggplot(swe_7_wl) +
  geom_sf(data = world, fill = "#EEDED1", color = "cornsilk4", size = 0.15) +
  geom_point(aes(long, lat)) +
  geom_label_repel(aes(long, lat, label = label, fill = if_else(tourn_w, "#FAC24CDA", "#FFFFFFDA"), size = if_else(venue == "Hong Kong", 4, 3)), family = f1, direction = "both", force = 60, point.padding = 0.5, max.overlaps = 100, min.segment.length = 0.2, segment.size = 0.2, segment.color = "grey50", label.size = 0, hjust = 0, seed = 2002) +
  # Title, subtitle and caption
  annotate("text", -45, 70, label = "Sweden women's national rugby sevens", family = f2b, fontface = "bold", hjust = 0, size = 11) +
  annotate("text", -38, 68, label = "Match results in tournaments 2001-2021, by venue and year. Tournaments won in golden", family = f2, hjust = 0, size = 6) +
  annotate("text", 61.5, 17.6, label = "Source: ScrumQueens · Graphic: Georgios Karamanis", family = f2, hjust = 1, size = 3) +
  scale_fill_identity() +
  scale_size_identity() +
  coord_sf(crs = "+proj=ortho +lat_0=40 +lon_0=8.1", default_crs = 4326, xlim = c(min(swe_7_wl$long) - 4.9, max(swe_7_wl$long) + 0.5), ylim = c(min(swe_7_wl$lat), max(swe_7_wl$lat)), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#B8CCCC", color = NA)
  )
  

p +
  inset_element(t, 0.82, 0.08, 1, 0.7)
