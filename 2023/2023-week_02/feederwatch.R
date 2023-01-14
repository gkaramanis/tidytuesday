library(tidyverse)
library(sf)
library(patchwork)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8.44, height = 10.5, units = "in", dpi = 320)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

us <- rgeoboundaries::gb_adm1("usa") %>% 
  st_crop(xmin = -125, xmax = -67, ymin = 23, ymax = 52) 

species <- "comgra"

birds_sf <- feederwatch %>% 
  janitor::clean_names() %>% 
  filter(species_code == species) %>%
  select(loc_id, month, longitude, species_code, latitude, how_many) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(4326) %>% 
  st_transform(st_crs(us))

birds_w <- birds_sf %>% 
  filter(month >= 9 | month <= 2)

birds_s <- birds_sf %>% 
  filter(month >= 3 & month <= 8)

hotspots_w <- sfhotspot::hotspot_gistar(birds_w, cell_size = 0.5, weights = how_many, kde = FALSE)

hotspots_s <- sfhotspot::hotspot_gistar(birds_s, cell_size = 0.5, weights = how_many, kde = FALSE)

f1 <- "Outfit"
f2 <- "Iosevka Fixed"

hotspot_plot <- function(df, title) {
  ggplot(df %>% filter(gistar > 0, pvalue < 0.05)) +
    geom_sf(data = us, fill = "#E5E6E8", color = "#868D94") +
    geom_sf(aes(colour = gistar, fill = gistar)) +
    scale_fill_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
    scale_color_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
    coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
    labs(title = title) +
    theme_void(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "#868D94", color = NA),
      plot.title = element_markdown(size = 18, color = "grey85"),
      legend.key.height = unit(2, "lines"),
      legend.key.width = unit(0.8, "lines"),
      legend.title = element_blank(),
      legend.text = element_text(size = 13, color = "white", family = f2)
    )
  }
  
img <- ggplot() +
  ggimage::geom_image(aes(x = 1, y = 10, image = here::here("2023/2023-week_02/img/grackle.png")), size = 0.5) +
  coord_fixed() +
  theme_void()

w <- hotspot_plot(hotspots_w, "<span style='color:white'>Winter</span> (Sep - Feb)")
s <- hotspot_plot(hotspots_s, "<span style='color:white'>Summer</span> (Mar - Aug)")

w / s +
  inset_element(img, 0.9, 0.95, 1.2, 1.95) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Observations of common grackle",
    subtitle = "Gi* value for cells with p-value ≥ 0.05",
    caption = "Source: Project FeederWatch ܍ Photo: Tina Nord ܍ Graphic: Georgios Karamanis",
    theme = theme(
      plot.margin = margin(10, 40, 10, 40),
      plot.background = element_rect(fill = "#868D94", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", family = f1, color = "white", size = 24, margin = margin(10, 0, 7, 0)),
      plot.subtitle = element_text(hjust = 0.5, family = f1, color = "white", size = 16, margin = margin(0, 0, 20, 0)),
      plot.caption = element_text(color = "#E5E6E8", family = f1)
      )
    ) 


