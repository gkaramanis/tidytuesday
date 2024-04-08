library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

eclipse_total_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')

us <- tigris::states(resolution = "20m")

tot_ecl <- eclipse_total_2024 %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(annularity_start = as.POSIXct(eclipse_3))

cities <- maps::us.cities %>% 
  mutate(name = str_remove(name, paste0(" ", country.etc))) %>% 
  rename(state = country.etc) %>% 
  left_join(tot_ecl) %>% 
  filter(!is.na(eclipse_3)) %>% 
  filter(pop > 150000) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

pal <- MetBrewer::met.brewer("Tam")

f1 <- "Graphik Compact"

ggplot() +
  geom_sf(data = us, fill = "#4B423D", color = "grey70") +
  geom_sf(data = tot_ecl, aes(color = annularity_start), size = 0.3) +
  geom_sf(data = cities, shape = 21) +
  ggrepel::geom_text_repel(data = cities, aes(geometry = geometry, label = name), stat = "sf_coordinates", family = f1, size = 4.5, force = 5, force_pull = 0, seed = 99, bg.color = "black", color = "white", point.padding = 0.17) +
  scale_color_stepsn(colors = pal, trans = "time") +
  coord_sf(crs = st_crs("ESRI:102003"), xlim = c(-2300000, 2200000), ylim = c(-1400000, 1600000)) +
  labs(
    title = "Total Solar Eclipse 2024",
    caption = "Source: NASA Scientific Visualization Studio · Graphic: Georgios Karamanis",
    color = "Start time (UTC) of total phase"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 13),
    plot.background = element_rect(fill = "#c1ced6", color = NA),
    plot.title = element_text(size = 28, hjust = 0.5, face = "bold", margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(20, 0, 10, 0)
  )
