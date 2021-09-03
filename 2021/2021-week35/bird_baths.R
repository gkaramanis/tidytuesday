library(tidyverse)
library(camcorder)
library(sf)
library(gggibbous)
library(ggrepel)
library(shadowtext)
library(ggnewscale)

gg_record(dir = "temp", device = "png", width = 10.5, height = 12, units = "in", dpi = 320)

bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

# bbox <- st_bbox(c(xmin = 134, ymin = -22, xmax = 155, ymax = -40))

ibra <- read_sf(here::here("2021", "2021-week35", "data", "IBRA7_regions_states", "IBRA7_regions_states.shp")) %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 0.1)  
  # st_crop(bbox)

bb_totals <- bird_baths %>% 
  filter(!is.na(bioregions)) %>% 
  group_by(urban_rural, bioregions) %>% 
  summarise(n = sum(bird_count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = urban_rural, values_from = n) %>% 
  mutate(
    total = Urban + Rural,
    bioregions = if_else(bioregions == "Victorian Volcanic Plain", "Southern Volcanic Plain", bioregions)
  ) %>% 
  pivot_longer(c("Rural", "Urban"), names_to = "urban_rural", values_to = "n") %>% 
  mutate(ratio = n / total)

ibra_sf <- ibra %>% 
  filter(REG_NAME_7 %in% bb_totals$bioregions) %>% 
  group_by(REG_NAME_7) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  rename(bioregions = REG_NAME_7)

ibra_centroids <- ibra_sf %>%
  st_centroid() %>% 
  left_join(bb_totals) %>% 
  cbind(st_coordinates(.))

ggplot() +
  # All bioregions
  geom_sf(data = ibra, fill = "grey85", color = "grey99", size = 0.25) +
  # Bioregions with sightings
  geom_sf(data = ibra_sf, fill = "grey72", color = "grey99", size = 0.25) +
  # Moons
  geom_moon(data = ibra_centroids, aes(X, Y, size = total, ratio = ratio, right = if_else(urban_rural == "Rural", TRUE, FALSE), fill = if_else(urban_rural == "Rural", "black", "white"))) +
  # Legend
  annotate("tile", x = 138, y = -25.3, width = 6.5, height = 4, fill = "#ffffff50", color = NA) +
  geom_moon(data = NULL, aes(x = 138, y = -26, ratio = 0.7), size = 30, right = FALSE) +
  geom_moon(data = NULL, aes(x = 138, y = -26, ratio = 0.3), size = 30, right = TRUE, fill = "black") +
  geom_shadowtext(data = NULL, aes(x = 138, y = -26, label = "Urban", size = 30), nudge_x = -0.25, hjust = 1, color = "black", bg.colour = "white", bg.r = 0.08) +
  geom_shadowtext(data = NULL, aes(x = 138, y = -26, label = "Rural", size = 30), nudge_x = 0.25, hjust = 0, color = "white", bg.colour = "black", bg.r = 0.08) +
  annotate("text", x = 138, y = -24.2, label = "Number of sightings\nby area", family = "Source Serif Pro", fontface = "bold", size = 7, lineheight = 0.9) +
  # Bioregion labels
  geom_label_repel(data = ibra_centroids, aes(x = if_else(str_detect(bioregions, "Coastal"), X + 1, X - 1), y = Y, label = bioregions, hjust = if_else(str_detect(bioregions, "Coastal"), 0, 1)), color = "grey20", point.padding = 30, family = "Source Serif Pro", size = 5, fontface = "bold", label.size = 0, fill = "#f8f8ff70", stat = "unique", nudge_y = 0.35) +
  scale_size_continuous(range = c(8, 25)) +
  # Number of sightings
  new_scale("size") +
  geom_shadowtext(data = ibra_centroids %>% filter(urban_rural == "Urban"), aes(X, Y, label = n, size = n), nudge_x = -0.25, hjust = 1, color = "black", bg.colour = "white", bg.r = 0.08) +
  geom_shadowtext(data = ibra_centroids %>% filter(urban_rural == "Rural"), aes(X, Y, label = n, size = n), nudge_x = 0.25, color = "white", hjust = 0, bg.r = 0.08, bg.colour = "black") +
  scale_size_continuous(range = c(4.5, 9)) +
  # scales, theme, etc
  scale_fill_identity() +
  coord_sf(clip = "off", xlim = c(133, 155), ylim = c(-22, -40), expand = FALSE) +
  labs(
    title = toupper("Bird sightings at bird baths"),
    subtitle = str_wrap("Started by researchers at Deakin University and Griffith University in 2014, the Bathing Birds Study involved collecting data online from 2,500 citizen scientists on bathing birds all over Australia."),
    caption = str_wrap("Data: P. Cleary, Gráinne; Parsons, Holly; Davis, Adrian; R. Coleman, Bill; N. Jones, Darryl; K. Miller, Kelly; et al. (2016) · Graphic: Georgios Karamanis")
    ) +
  theme_void(base_family = "Source Serif Pro") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(size = 30, hjust = 0, family = "Faune", face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0),
    plot.caption = element_text(hjust = 0, size = 11),
    plot.margin = margin(10, 20, 0, 20)
  )


# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # Alfred

# ggsave(here::here("temp", paste0("bird_baths-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

