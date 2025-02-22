library(tidyverse)
library(sf)
library(spatstat)
library(camcorder)
library(patchwork)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

agencies_sf <- agencies %>% 
  filter(!is.na(longitude)) %>% 
  filter(longitude < 0 & latitude > 0) %>%
  filter(!is.na(agency_type) & agency_type != "Unknown") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3857)  # Transform to Web Mercator

# Create counts by agency type
agency_counts <- agencies_sf %>%
  st_drop_geometry() %>% 
  filter(agency_type != "Other") %>%
  count(agency_type) %>%
  mutate(label = glue::glue("{agency_type}\n({scales::comma(n)} agencies)"))

us_sf <- rnaturalearth::countries110 %>% 
  filter(ADMIN == "United States of America") %>% 
  st_as_sf() %>% 
  st_transform(3857)  # Transform to Web Mercator

north_america_sf <- rnaturalearth::countries110 %>% 
  filter(ADMIN %in% c("United States of America", "Canada", "Mexico")) %>% 
  st_as_sf()

agencies_density_all <- agencies_sf %>%
  filter(agency_type != "Other") %>%
  split(.$agency_type) %>%
  map_dfr(function(x) {
    as.ppp(x$geometry, W = as.owin(us_sf)) %>%
    density(dimyx = 300) %>%
    stars::st_as_stars() %>%
    st_as_sf() %>%
    st_set_crs(3857) %>%
    mutate(
      agency_type = unique(x$agency_type),
      v = (v - min(v)) / (max(v) - min(v))  # normalize to 0-1
    )
  }) %>% 
  left_join(agency_counts, by = "agency_type")

pal <- rev(MetBrewer::met.brewer("Tam"))

f1 <- "Plus Jakarta Sans"
f2 <- "Montagu Slab 16pt"


ggplot(agencies_density_all) +
  geom_sf(data = north_america_sf, fill = "grey97", color = "grey55", linewidth = 0.1) + 
  geom_sf(aes(fill = v, color = v), linewidth = 0.1) +
  scale_color_stepsn(colors = pal, breaks = seq(0, 1, 0.1)) +
  scale_fill_stepsn(colors = pal, breaks = seq(0, 1, 0.1)) +
  coord_sf(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", xlim = c(-6e6, 2.6e6), ylim = c(-1.2e6, 4.6e6)) +
  labs(
    title = "Mapping America's Law Enforcement Network: 19,000+ Agencies Across the Nation",
    subtitle = str_wrap("Heat map showing the geographic concentration of different types of law enforcement agencies that report to the FBI's Uniform Crime Reporting (UCR) Program. Each panel displays normalized density values (0-1) to highlight regional patterns, regardless of the total number of agencies in each category.", 135),
    caption = "Source: FBI Crime Data API · Graphic: Georgios Karamanis",
    fill = "Density",
    color = "Density"
    ) +
  facet_wrap(vars(label), ncol = 3) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.margin = margin(0, 0, 10, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = NA, color = "grey55", linewidth = 0.2),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = f2, size = 17, face = "bold"),
    plot.subtitle = element_text(family = f1, size = 13, margin = margin(6, 0, 10, 0)),
    strip.text = element_text(margin = margin(0, 0, 5, 0))
  )