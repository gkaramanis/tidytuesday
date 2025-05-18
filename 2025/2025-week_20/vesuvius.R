library(tidyverse)
library(elevatr)
library(ggpointdensity)
library(marmap)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')

# Get elevation data
vesuvius_hm <- vesuvius %>% 
  filter(!is.na(longitude)) %>% 
  select(x = longitude, y = latitude) %>% 
  as.data.frame() %>% 
  get_elev_raster(
    locations = ., 
    z = 13,
    prj = 4326
  ) 

# Convert elevation raster to data frame
vesuvius_hm_df <- terra::as.data.frame(vesuvius_hm, xy = T) %>%
  na.omit() %>% 
  rename(elevation = 3)

# Create a diagonalsection and get the elevation
p1_x <- 14.34
p1_y <- 40.785
p2_x <- 14.50
p2_y <- 40.85

# Tolerance for getting the points on the line
epsilon <- 5e-6

# Calculate the cross product to find points on the line
v_section <- vesuvius_hm_df %>%
  mutate(
    cross_product = (y - p1_y) * (p2_x - p1_x) - (x - p1_x) * (p2_y - p1_y)
  ) %>%
  filter(
    abs(cross_product) < epsilon &
      x >= (min(p1_x, p2_x) - epsilon) & x <= (max(p1_x, p2_x) + epsilon) &
      y >= (min(p1_y, p2_y) - epsilon) & y <= (max(p1_y, p2_y) + epsilon)
  ) 

# Rescale the x axis to use in plot
v_section_t <- v_section %>% 
  mutate(
    x = scales::rescale(x, c(min(vesuvius$time), max(vesuvius$time)))
    )

# Annotations
annot <- tribble(
  ~time, ~depth_km, ~label,
  as_datetime("2021-06-01"), 0.2, "2018–2021 and 2024 have been most active years in recent history",
  as_datetime("2023-10-01"), -8.8, "The deepest activity was recorded on\nApril 14, 2024, at 9.35 km",
  as_datetime("2016-09-15"), 1.1, "Vesuvius' maximum height is\n1 281 m above sea level",
)

f1 <- "Charter"
f2 <- "Asap Semi Condensed"

# Main plot
p <- ggplot() +
  # Vesuvius
  geom_segment(data = v_section_t, aes(x = x, xend = x, y = min(elevation / 1000), yend = elevation / 1000), color = "#eed9c4") +
  # Points
  geom_pointdensity(data = vesuvius, aes(time, -depth_km), size = 0.25) +
  # Annotations
  shadowtext::geom_shadowtext(data = annot, aes(time, depth_km, label = label), family = f2, lineheight = 0.9, color = "black", bg.color = "white") +
  annotate("segment", x = as_datetime("2018-01-10"), xend = as_datetime("2018-11-01"), y = 1.25, color = "#d9b99b", linewidth = 0.4) +
  annotate("point", x = as_datetime("2018-11-01"), y = 1.25, color = "#d9b99b", size = 0.8) +
  # Scales, labs, theme
  MetBrewer::scale_color_met_c("Johnson", direction = -1) +
  scale_x_datetime(minor_breaks = "1 year", expand = c(0.01, 0), position = "top") +
  scale_y_continuous(labels = function(x) scales::number(abs(x), scale = 1e3), position = "right") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Vesuvius seismic activity",
    subtitle = str_wrap("Seismic activity recorded at Mount Vesuvius 2011–2024. The main chart displays the temporal distribution of seismic events and their corresponding depths, set against a profile of the volcano for scale. The inset contour map shows the location of the events.", 132),
    caption = "Source: Istituto nazionale di geofisica e vulcanologia · Graphic: Georgios Karamanis",
    x = "Year",
    y = "Depth in meters"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major = element_line(color = "darkgrey", size = 0.1),
    panel.grid.minor = element_line(color = "darkgrey", size = 0.04),
    axis.text = element_text(size = 11),
    plot.title = element_text(size= 18, face = "bold"),
    plot.subtitle = element_text(size = 12, lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), size = 10)
  )

# Create Italy map with Vesuvius point
italy <- rnaturalearth::ne_countries(country = "italy", scale = 50)

vesuvius_point <- vesuvius_hm_df %>% 
  summarise(x = median(x), y = median(y)) 

# Map plot
m <- ggplot(italy) +
  geom_sf(fill = "#eed9c4", color = "#d9b99b") +
  geom_point(data = vesuvius_point, aes(x, y), size = 2, color = "red2") +
  geom_text(data = vesuvius_point, aes(x, y), family = f2, nudge_y = 0.8, label = "Mount Vesuvius", size = 3) +
  geom_text(data = vesuvius_point, aes(x, y), family = f2, nudge_x = -4, nudge_y = 4.5, label = "ITALY", fontface = "bold") +
  theme_void()

# Contour plot
c <- ggplot() +
  geom_contour(data = vesuvius_hm_df, aes(x = x, y = y, z = elevation), bins = 30, color = "#d9b99b", linewidth = 0.2) +
  ggpointdensity::geom_pointdensity(data = vesuvius, aes(longitude, latitude), size = 0.1, method = "kde2d") +
  MetBrewer::scale_color_met_c("Johnson", direction = -1) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#f5f5f5", color = "darkgrey")
  )

# Combine plots
p + 
  inset_element(m, 0, 0, 0.35, 0.35) +
  inset_element(c, 0.3, -0.05, 0.6, 0.35) &
  theme(
    plot.background = element_rect(fill = "#f5f5f5", color = NA)
  )

