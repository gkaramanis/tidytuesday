library(tidyverse)
library(sf)
library(geosphere)
library(ggpmisc)
library(shadowtext)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

orcas <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv')

orcas_with_bearing <- orcas %>% 
  mutate(month = month(date)) %>% 
  filter(!is.na(month)) %>% 
  mutate(
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      month %in% c(12, 1, 2) ~ "Winter"
    )
  ) %>% 
  filter(!is.na(begin_longitude) & !is.na(begin_latitude) &  !is.na(end_longitude) & !is.na(end_latitude)) %>%
  mutate(bearing = bearing(cbind(begin_longitude, begin_latitude), cbind(end_longitude, end_latitude))) 

us_ca_map <- rgeoboundaries::gb_adm0(country = c("USA", "Canada")) %>% 
  rmapshaper::ms_simplify(0.5)

orcas_sf <- orcas_with_bearing %>% 
  st_as_sf(coords = c("begin_longitude", "begin_latitude"), crs = 4326)

proj <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Function to create circular histogram
create_circular_histogram <- function(season) {
  ggplot(orcas_with_bearing %>% filter(season == !!season)) +
    geom_hline(yintercept = c(5, 10, 15), linewidth = 0.05, color = "brown4") +
    annotate("shadowtext", x = 60, y = c(5, 10, 15), label = c(5, 10, 15), family = f1b, size = 2.5, color = "brown4", bg.color = "#f5fafa") +
    geom_histogram(aes(x = bearing, fill = after_stat(x)), binwidth = 10) +
    MetBrewer::scale_fill_met_c("Ingres") +
    scale_x_continuous(breaks = seq(-180, 180, 45)) +
    coord_polar(start = -pi) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = NA, color = NA)
    )
}

# Create a data frame for histograms
histograms <- data.frame(
  season = c("Winter", "Spring", "Summer", "Fall"),
  x = -125.55,
  y = 47.65
) %>% 
  rowwise %>% 
  mutate(plot = list(create_circular_histogram(season)))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

p <- ggplot() +
  geom_sf(data = us_ca_map, color = "#cbc5c2", fill = "#eae8e6", linewidth = 0.15) +
  geom_segment(data = orcas_with_bearing, aes(x = begin_longitude, y = begin_latitude, xend = end_longitude, yend = end_latitude, color = bearing), arrow = arrow(length = unit(0.2, "cm")), alpha = 0.7, linewidth = 0.2) +
  geom_text(data = orcas_with_bearing, aes(-124.9, 47.7, label = season), stat = "unique", family = f1b, fontface = "bold", size = 7, alpha = 0.5, color = "orange4") +
  geom_plot(data = histograms, aes(x = x, y = y, label = plot), vp.width = 0.35, vp.height = 0.35) +
  MetBrewer::scale_color_met_c("Ingres") +
  coord_sf(crs = proj, default_crs = 4326, xlim = c(st_bbox(orcas_sf)$xmin, st_bbox(orcas_sf)$xmax + 0.3), ylim = c(st_bbox(orcas_sf)$ymin, st_bbox(orcas_sf)$ymax - 0.8)) +
  facet_wrap(vars(factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))) +
  labs(
    title = "Seasonal Orca movements in the Salish Sea",
    subtitle = str_wrap("The map shows orca movements in the Salish Sea across four seasons (2017-2024). Arrows connect start and end points of sightings, with colors indicating travel direction. Circular histograms display directional distribution per season.", 120),
    caption = "Source: Center for Whale Research · Graphic: Georgios Karamanis",
    color = "Bearing (degrees)"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = "black"),
    panel.background = element_rect(fill = "#f5fafa", color = NA),
    panel.border = element_rect(fill = NA),
    strip.text = element_blank(),
    plot.title = element_text(family = f2, face = "bold", size = 18),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0)),
    plot.margin = margin(10, 10, 10, 10)
  ) 
  
# Inset
inset_map <- ggplot() +
  geom_sf(data = us_ca_map, color = "#C6C6C6", fill = "#E8E8E8", linewidth = 0.15) +
  geom_sf(data = st_as_sfc(st_bbox(orcas_sf)), color = "coral", fill = NA, linewidth = 0.5) +
  annotate("text", x = -128, y = 45, label = "Salish Sea", family = f1, size = 3) +
  annotate("text", x = -118, y = 54, label = "Canada", family = f1, size = 3.5, color = "grey60") +
  annotate("text", x = -115, y = 43, label = "United States", family = f1, size = 3.5, color = "grey60") +
  coord_sf(crs = proj, default_crs = 4326, xlim = c(st_bbox(orcas_sf)$xmin - 15, st_bbox(orcas_sf)$xmax + 0.3 + 15), ylim = c(st_bbox(orcas_sf)$ymin - 10, st_bbox(orcas_sf)$ymax - 0.8 + 10)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#F9F9F9", color = NA),
    panel.border = element_rect(fill = NA)
  )

p + inset_element(inset_map, left = 0.8, right = 1.052, top = 1, bottom = 0.8, align_to = "full")
