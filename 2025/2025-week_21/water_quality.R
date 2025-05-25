library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 9, units = "in", dpi = 320)

water_quality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv')

weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/weather.csv')

aus <- rnaturalearth::ne_countries(country = "Australia", scale = 10) %>% 
  st_transform(crs = 3577)

bbox_wkt <- "POLYGON((150 -34.2, 151.6 -34.2, 151.6 -33.5, 150 -33.5, 150 -34.2))"

bbox_wkt_transformed <- st_as_sfc(bbox_wkt, crs = 4326) %>% 
  st_transform(bbox_sf, crs = 3577) %>% # GDA94 / Australian Albers
  st_as_text()

# Download file from https://data.dea.ga.gov.au/derivative/dea_waterbodies/3-0-0/shapefile/ga_ls_wb_3_v3.zip
aus_w <- st_read(here::here("2025/2025-week_21/data/ga_ls_wb_3_v3/"), wkt_filter = bbox_wkt_transformed)

ww <- water_quality %>% 
  left_join(weather, by = "date") %>% 
  group_by(swim_site, longitude.x, latitude.x) %>% 
  summarise(
    c_wt = cor(water_temperature_c, enterococci_cfu_100ml, method = "spearman", use = "pairwise.complete.obs"),
    c_pr = cor(precipitation_mm, enterococci_cfu_100ml, method = "spearman", use = "pairwise.complete.obs")
  ) %>% 
  ungroup() %>% 
  pivot_longer(c_wt:c_pr, names_to = "cor", values_to = "value") %>% 
  st_as_sf(coords = c("longitude.x", "latitude.x"), crs = 4326)

col_w1 <- scales::col_saturate("#5abcd8", -20)
col_w2 <- scales::col_saturate("#74ccf4", -20)
col_l <- scales::col_saturate("#8F705E", -15) %>% scales::col_lighter(amount = 5)

f1 <- "Outfit"
f2 <- "Bricolage Grotesque 12pt Condensed"

ggplot() +
  geom_sf(data = aus, fill = col_l, color = NA) +
  geom_sf(data = aus_w, fill = col_w1, color = col_w2) +
  geom_sf(data = ww %>% filter(value < 0.4), aes(size = value), color = "#F0DC80", alpha = 0.8) +
  geom_sf(data = ww %>% filter(value >= 0.4), aes(size = value), color = "#FA611F", alpha = 0.8) +
  ggrepel::geom_text_repel(data = ww %>% filter(value >= 0.2), aes(geometry = geometry, label = paste0(swim_site, "\n", round(value, 2))), color = "white", bg.color = "black", stat = "sf_coordinates", min.segment.length = 0, segment.size = 0.4, family = f2, fontface = "bold", lineheight = 0.9, force_pull = 0, seed = 99) +
  scale_size_area(max_size = 3) +
  facet_wrap(vars(cor), ncol = 1, labeller = labeller(cor = c(c_pr = "Precipitation", c_wt = "Water temperature"))) +
  coord_sf(xlim = c(149.9, 151.7), ylim = c(-34.15, -33.5), default_crs = 4326, expand = FALSE) +
  labs(
    title = "Water Quality in Sydney beaches",
    subtitle = str_wrap("Spearman correlation between enterococci levels and precipitation or water temperature. Most swimming sites have a weak or very weak positive correlation, while Yosemite Creek shows a moderate positive correlation with water temperature.", 126),
    caption = "Source: Beachwatch (NSW Government) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = col_w2, color = NA),
    strip.text = element_text(margin = margin(0, 0, 2, 0), size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(3, 0, 5, 0)),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.margin = margin(5, 0, 5, 0)
  )
