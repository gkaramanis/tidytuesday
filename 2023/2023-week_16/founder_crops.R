library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10.5, height = 8.5, units = "in", dpi = 320)

founder_crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv')

world <- rgeoboundaries::gb_adm0()

long_min <- min(founder_crops$longitude)
long_max <- max(founder_crops$longitude)
lat_min <- min(founder_crops$latitude)
lat_max <- max(founder_crops$latitude)

world_cropped <- world %>% 
  st_make_valid() %>% 
  st_crop(xmin = long_min - 2,
          xmax = long_max + 2,
          ymin = lat_min - 1,
          ymax = lat_max + 1)

world_ortho <- world %>% 
  st_transform(crs = "+proj=ortho +lon_0=45 +lat_0=40")

rect <- st_as_sf(data.frame(lat = 35, long = 36), coords = c("lat", "long"), crs = 4326)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lon_0=45 +lat_0=40")

rect <- tibble(lon = c(long_min - 1, long_max + 1), lat = c(lat_min - 1, lat_max + 1)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

f1 <- "Domine"
pal <- rev(MetBrewer::met.brewer("Homer2"))

p <- ggplot(founder_crops %>% filter(!is.na(category))) +
  geom_sf(data = world_cropped, fill = "grey97", color = "grey80") +
  ggpointdensity::geom_pointdensity(aes(longitude, latitude, size = n), shape = 21) +
  scale_size_continuous(range = c(0.5, 10), guide = guide_legend(label.position = "bottom", title.position = "top"), breaks = c(0, 25e6, 50e6, 75e6, 125e6), labels = c("< 1 M", "25 M", "50 M", "75 M", "125 M")) +
  scale_color_stepsn(colors = pal, guide = guide_colorsteps(title.position = "top")) +
  coord_sf(xlim = c(long_min, long_max), ylim = c(lat_min, lat_max)) +
  facet_wrap(vars(category)) +
  labs(
    title = "Revisiting the concept of the ‘Neolithic Founder Crops’ in southwest Asia",
    subtitle = "Sites and samples by category",
    caption = 'Source: The "Neolithic Founder Crops" in Southwest Asia: Research Compendium · Graphic: Georgios Karamanis',
    color = "Number of samples in the area",
    size = "Number of individuals in the sample"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(0, 10, 10, 10),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(margin = margin(0, 0, 5, 0), face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(5, 0, 30, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(10, 0, 0, 0))
  )

g <- ggplot(world_ortho) +
  geom_sf(data = ocean, color = NA, fill = "#EEF2F6") +
  geom_sf(fill = "grey20", color = "grey99", linewidth = 0.1) +
  geom_sf(data = rect, fill = NA, color = "red2", linewidth = 0.5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  )
  
p +
  inset_element(g, 0.75, 1.05, 1.05, 1.35) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

