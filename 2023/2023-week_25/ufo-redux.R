library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12.5, height = 10, units = "in", dpi = 320)

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')

projection <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

ufo_places <- ufo_sightings %>% 
  left_join(places) %>% 
  mutate(year = year(reported_date_time_utc)) %>% 
  add_count(year, name = "year_n")
  
p_ufo <- ggplot(ufo_places) +
  ggpointdensity::geom_pointdensity(aes(longitude, latitude), size = 0.1) 

ufo_density <- ggplot_build(p_ufo)$data[[1]] %>% 
  st_as_sf(crs = 4326, coords = c("x", "y"))

world <- rgeoboundaries::gb_adm0()

f1 <- "Outfit"

col_white <- "#F2F3F4"
col_bg <- "#010613"

p <- ggplot(ufo_density) +
  geom_sf(data = world, fill = "#2d2928", color = col_bg) +
  geom_sf(aes(color = n_neighbors), size = 0.2) +
  # scale_color_gradientn(colors = cetcolor::cet_pal("l7", n = 10)[2:10], labels = scales::number_format()) +
  scale_color_gradientn(colors = scico::scico(palette = "imola", n = 10), labels = scales::number_format(), name = "Sightings") +
  coord_sf(crs = projection) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.08, 0.3),
    legend.key.height = unit(1.8, "lines"),
    legend.key.width = unit(0.6, "lines"),
    legend.title = element_text(color = col_white),
    legend.text = element_text(color = col_white),
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.margin = margin(20, 0, 90, 0)
  )
  
l <- ggplot(ufo_places, aes(year, year_n)) +
  geom_line(stat = "unique", color = col_white) +
  geom_point(stat = "unique", color = col_white, size = 1, shape = 21, aes(fill = if_else(year %% 10 == 0, col_white, col_bg))) +
  annotate("text", 1925, 1000, label = "Number of sightings per year", family = f1, hjust = 0, color = col_white) +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(1920, 2020, 10), expand = c(0.015, 0.015)) +
  scale_y_continuous(position = "right", breaks = seq(0, 6000, 2000), labels = scales::number_format()) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    axis.text = element_text(color = "grey85", size = 9),
    axis.title = element_blank(),
    plot.margin = margin(0, 10, 10, 10),
    axis.ticks = element_line(color = "grey85"),
    panel.grid = element_blank()
  )

p +
  inset_element(l, bottom = -0.18, top = 0.15, left = 0, right = 1) +
  plot_annotation(
    title = "UFO sightings, 1925-2023",
    subtitle = "Source: National UFO Reporting Center · Graphic: Georgios Karamanis",
    theme = theme_void(base_family = f1) &
      theme(
        plot.background = element_rect(fill = col_bg, color = NA),
        plot.title = element_text(color = col_white, face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey85"),
        plot.margin = margin(10, 15, 10, 15)
      )
  ) 
  
