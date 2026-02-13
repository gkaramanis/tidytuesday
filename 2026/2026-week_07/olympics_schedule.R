library(tidyverse)
library(sf)
library(elevatr)
library(tidyterra)
library(ggrepel)
library(marquee)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)

# Fonts and colors
f1 <- "Graphik Compact"
f2 <- "Bricolage Grotesque"

col_point <- "#FFB81C"
col_accent <- "#FF6B35"
col_bg_light <- "#F4FAFC"
col_bg_alt <- "#FFF9F5"
col_bg_mid <- "#F5EDE5"
col_border <- "#A8998F"

# Read in data
schedule <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-10/schedule.csv')

it <- eurostat::get_eurostat_geospatial(country = "Italy", nuts_level = 3, resolution = 3)

# Venue locations
venues <- tribble(
  ~venue_name, ~venue_code, ~longitude, ~latitude,
  "Stelvio Ski Centre", "SSC", 10.3867, 46.5142,
  "Tofane Alpine Skiing Centre", "CAL", 12.0981, 46.5286,
  "Cortina Curling Olympic Stadium", "CCU", 12.1356, 46.5406,
  "Milano Rho Ice Hockey Arena", "MH2", 9.0770, 45.5231,
  "Milano Santagiulia Ice Hockey Arena", "MH1", 9.2706, 45.4272,
  "Cortina Sliding Centre", "CSC", 12.1631, 46.5431,
  "Predazzo Ski Jumping Stadium", "PSJ", 11.6017, 46.3122,
  "Livigno Snow Park", "LSP", 10.1372, 46.5356,
  "Milano Ice Skating Arena", "MSK", 9.2044, 45.4840,
  "Tesero Cross-Country Skiing Stadium", "TCC", 11.5056, 46.2992,
  "Milano Speed Skating Stadium", "MSS", 9.0931, 45.5167,
  "Anterselva Biathlon Arena", "ABA", 12.1592, 46.8642,
  "Livigno Aerials & Moguls Park", "LAM", 10.1372, 46.5356
) |> 
  mutate(city = str_extract(venue_name, "^[A-Za-z]+")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(it), remove = FALSE) %>%
  st_join(it %>% select(NUTS_ID, NAME_LATN), join = st_within) %>%
  mutate(region_code = NUTS_ID) %>%
  select(-NUTS_ID) %>%
  st_drop_geometry()

# Aggregated event counts by venue and region
venues_events <- schedule |> 
  filter(is_medal_event, !is.na(venue_code)) |> 
  left_join(venues) |> 
  count(longitude, latitude, venue_name, city, region_code, venue_code, discipline_name, name = "n") |> 
  group_by(longitude, latitude, venue_name, city, region_code, venue_code) |> 
  summarize(venue_block = str_c("**", first(venue_name), "**", "  \n", str_c(paste(discipline_name, n), collapse = "  \n")), .groups = "drop") |> 
  group_by(longitude, latitude) |> 
  summarize(
    label = str_c(venue_block, collapse = "  \n"),
    city = first(city),
    region_code = first(region_code),
    .groups = "drop"
  ) |> 
  group_by(region_code) |> 
  arrange(longitude) |> 
  mutate(label_order = row_number()) |> 
  ungroup()

# City labels
cities <- venues |> 
  group_by(city) |> 
  summarise(
    longitude = mean(longitude),
    latitude = mean(latitude)
  ) |> 
  ungroup() |> 
  inner_join(venues |> select(city, region_code, NAME_LATN), by = "city")

# Region plots
region_plot <- function(region_code) {
  region_sf <- it |> filter(NUTS_ID == region_code)
  region_elev <- get_elev_raster(region_sf, z = 8, clip = "locations")
  region_venues <- venues |> filter(region_code == !!region_code)
  region_events <- venues_events |> filter(region_code == !!region_code)

  ggplot(region_sf) +
    geom_sf(fill = col_bg_alt, color = NA) +
    geom_spatraster(data = region_elev |> as("SpatRaster"), aes(alpha = after_stat(value))) +
    geom_sf(fill = NA, color = col_border, linewidth = 0.35) +
    geom_marquee_repel(data = region_events |> filter(label_order == 1), aes(x = longitude, y = latitude, label = label), stat = "unique", size = 2, direction = "x", nudge_x = -0.65, width = 0.4, segment.size = 0.2, segment.color = col_accent) +
    geom_marquee_repel(data = region_events |> filter(label_order == 2), aes(x = longitude, y = latitude, label = label), stat = "unique", size = 2, direction = "y", nudge_y = -0.18, width = 0.5,  segment.size = 0.2, segment.color = col_accent) +
    geom_marquee_repel(data = region_events |> filter(label_order == 3), aes(x = longitude, y = latitude, label = label), stat = "unique", size = 2, direction = "y", nudge_y = 0.25, hjust = 0, width = 0.5,  segment.size = 0.2, segment.color = col_accent) +
    geom_point(data = region_events, aes(x = longitude, y = latitude), fill = col_point, color = col_accent, shape = 21, alpha = 0.9) +
    geom_text_repel(data = cities |> filter(region_code == !!region_code), aes(x = longitude, y = latitude, label = city), stat = "unique", size = 3, segment.color = col_accent, segment.alpha = 0.9, segment.size = 0.1, family = f1, fontface = "bold") +
    scale_fill_gradientn(colors = gray.colors(100, start = 0.2, end = 0.8, rev = TRUE), guide = "none", na.value = NA) +
    scale_alpha_continuous(range = c(0.1, 0.45), guide = "none") +
    coord_sf(clip = "off", expand = TRUE) +
    labs(title = region_sf$NAME_LATN) +
    theme_void(base_family = f1) +
    theme(
      plot.background = element_rect(fill = col_bg_light, color = NA),
      plot.title = element_text(face = "bold", family = f2)
    )
}

region_codes <- venues |> 
  arrange(longitude, latitude) |> 
  distinct(region_code) |> 
  pull(region_code) 
region_plots <- map(region_codes, region_plot)

# Combine Italy map and region maps
label_style <- classic_style() |> 
  modify_style("city", family = f1) |>
  modify_style("region", family = f2, size = 7)

p0 <- ggplot(it) +
  geom_sf(fill = NA, color = col_border, linewidth = 0.5) +
  geom_sf(aes(fill = id %in% venues$region_code), color = col_border, linewidth = 0.1) +
  geom_marquee_repel(data = cities, aes(x = longitude, y = latitude, label = paste0("{.city **", city, "**}", "  \n", "{.region ", NAME_LATN, "}")), stat = "unique", size = 3, segment.color = col_accent, segment.alpha = 0.9, segment.size = 0.1, family = f1, lineheight = 0.7, style = label_style) +
  geom_point(data = venues, aes(x = longitude, y = latitude), fill = col_point, color = col_accent, shape = 21, alpha = 0.9) +
  scale_y_continuous(limits = c(36.5, 49)) +
  scale_fill_manual(values = c(col_bg_alt, col_bg_mid), guide = "none") +
  labs(
    title = "2026 Winter Olympics",
    subtitle = str_wrap("Main map ↓ shows the eight cities hosting Olympic venues and their regions. Detail maps → show medal event counts by venue in each area.", 70),
    caption = "Source: Olympics.com, Wikipedia & Kaggle · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = col_bg_light, color = NA),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, margin = margin(7, 0, 0, 0), hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, margin = margin(-10, 0, 10, 0))
  )


free(p0) + 
  wrap_plots(region_plots, ncol = 2, widths = c(1, 1)) &
  theme(
      plot.background = element_rect(fill = col_bg_light, color = NA)
    ) 

