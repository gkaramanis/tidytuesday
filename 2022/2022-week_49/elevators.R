library(tidyverse)
library(sf)
library(patchwork)
library(ggrepel)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 320)

elevators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv') %>% 
  janitor::clean_names()

manh_pass <- elevators %>% 
  filter(longitude > -77) %>% 
  filter(borough == "Manhattan") %>% 
  filter(device_type == "Passenger Elevator (P)")

manhattan <- read_sf(here::here("2022/2022-week_49/data/manhattan.geojson"))  

f1 <- "Outfit"

manh_pass_sf <- st_as_sf(manh_pass, coords = c("longitude", "latitude"), 
         crs = 4326)

joined <- st_join(manhattan %>% st_make_valid(), manh_pass_sf)

elev_n <- joined %>% 
  count(name, geometry) %>% 
  mutate(label = case_when(
    !is.na(name) ~ str_wrap(paste0(name, ": ", n), 20),
    TRUE ~ as.character(n)
  ))

# Circle crop
c_crop <- st_as_sfc("POINT(-73.98 40.755)", crs = 4326) %>% 
  st_buffer(dist = 1800) 

elev_n_cropped <- st_intersection(elev_n, c_crop)

f1 <- "Outfit"
pal <- scico::scico(n = 10, palette = "roma", direction = -1)

m <- ggplot(elev_n) +
  geom_sf(aes(fill = n), linewidth = 0.1, color = NA) +
  geom_text_repel(data = elev_n %>% filter(n >= 40 & !(name %in% elev_n_cropped$name)), aes(geometry = geometry, label = label), color = "white", family = f1, bg.color = "#293327", stat = "sf_coordinates", size = 3.5, lineheight = 0.9, min.segment.length = 0, segment.color = "black", segment.size = 0.15) +
  geom_sf(data = st_centroid(elev_n %>% filter(n > 40 & !(name %in% elev_n_cropped$name))), color = "black", size = 0.1, alpha = 0.5) +
  annotate("text", x = -73.89, y = 40.875, label = "Manhattan\nElevators", size = 20, family = f1, color = "white", hjust = 0, vjust = 1, lineheight = 0.9, fontface = "bold") +
  annotate("text", x = -73.89, y = 40.84, label = "Number of elevators by building", size = 7.5, family = f1, color = "white", hjust = 0, vjust = 1, lineheight = 0.9) +
  annotate("text", x = -73.89, y = 40.831, label = "Source: elevators package & NYC Open Data\nGraphic: Georgios Karamanis", size = 4.4, family = f1, color = "white", hjust = 0, vjust = 1) +
  scale_fill_stepsn(colors = pal, breaks = seq(0, 80, 10)) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#96A593", color = NA)
  )

z <- ggplot(elev_n_cropped) +
  geom_sf(aes(fill = n), linewidth = 0.1, color = NA) +
  geom_text_repel(data = elev_n_cropped %>% filter(n >= 40), aes(geometry = geometry, label = label), color = "white", family = f1, bg.color = "#293327", stat = "sf_coordinates", min.segment.length = 0, segment.color = "black", segment.size = 0.15) +
  geom_sf(data = st_centroid(elev_n_cropped %>% filter(n > 40 & !is.na(name))), color = "black", size = 0.1, alpha = 0.5) +
  scale_fill_stepsn(colors = pal, breaks = seq(0, 80, 10)) +
  coord_sf(xlim = c(-74, -73.96), ylim = c(40.74, 40.77)) +
  theme_void()


m +
  inset_element(z, 0.55, 0.05, 2.3, 0.6) +
  plot_layout(guides = "collect") &
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(4, "line"),
    legend.key.height = unit(0.7, "line"),
    plot.background = element_rect(fill = "#96A593", color = NA),
    plot.margin = margin(0, 150, 10, 0)
  )
