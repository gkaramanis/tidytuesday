library(tidyverse)
library(sf)
library(camcorder)
library(patchwork)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

scottish_munros <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv')

sm_20 <- scottish_munros |> 
  filter(!is.na(`2021`)) |> 
  slice_max(Height_m, n = 20) |> 
  mutate(Name = fct_reorder(Name, Height_m, .desc = TRUE)) |> 
  arrange(-Height_m) |> 
  mutate(i = row_number())

scotland <- read_sf("https://raw.githubusercontent.com/Crunch-io/geodata/refs/heads/master/UK-GeoJSON-part/json/eurostat/simplified/scotland.geojson")

sm_sf <- scottish_munros |> 
  filter(!is.na(xcoord)) |> 
  st_as_sf(coords = c("xcoord", "ycoord"), crs = "EPSG:27700") |> 
  st_transform(crs = st_crs(scotland))

munro_gradient <- grid::linearGradient(c("darkgreen", "orange2"), group = FALSE)

f1 <- "Publico Headline"
f2 <- "DIN Condensed"

m <- ggplot() +
  geom_sf(data = scotland, fill = "#D2B48C", color = NA, alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = sm_sf, size = 0.1, color = "grey30") +
  geom_sf(data = sm_sf %>% filter(Height_m == max(Height_m)), color = "red", size = 1.5) +
  ggrepel::geom_text_repel(data = sm_sf %>% filter(Height_m == max(Height_m)), aes(label = Name, geometry = geometry), stat = "sf_coordinates", family = f1, nudge_x = -7, size = 3, segment.size = 0.1) +
  coord_sf(expand = FALSE) +
  theme_void()

p <- ggplot(data = sm_20, aes(x = Name, y = Height_m)) +
  geom_col(fill = munro_gradient) +
  ggpattern::geom_col_pattern(data = . %>% filter(`2021` == "Munro Top"), fill = NA, pattern_size = 0.05, pattern_colour = NA, pattern_fill = "orange", pattern_spacing = 0.005) +
  geom_text(aes(y = 30, label = paste0(i, ". ", Name)), angle = 90, hjust = 0, color = "white", family = f1, fontface = "bold", size = 5.5) +
  geom_text(aes(label = if_else(Height_m == max(Height_m), paste0(scales::number(Height_m, accuracy = 1), " m"), scales::number(Height_m, accuracy = 1))), nudge_y = 30, family = f2, size = 4.5, color = "#343D34") +
  geom_text(aes(label = if_else(Height_ft == max(Height_ft), paste0(scales::number(Height_ft, accuracy = 1), " ft"), scales::number(Height_ft, accuracy = 1))), nudge_y = 72, family = f2, size = 4.5, color = "#817A6E") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "The 20 highest Munros and Munro tops in Scotland",
    subtitle = "A Munro is a mountain in Scotland that is at least 3,000 feet, or 914.4 meters, high. A Munro top, shown in the chart with a striped pattern fill, is a subsidiary summit that also exceeds 3,000 feet in height but is not considered a distinct mountain.",
    caption = "Data: The Database of British and Irish Hills v18.2 · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 24, family = f1, face = "bold", hjust = 0),
    plot.subtitle = marquee::element_marquee(size = 15, family = f1, hjust = 0, margin = margin(5, 0, 35, 0), width = 0.88)
  )

p + 
  inset_element(m, left = 0.7, bottom = 0.93, right = 1.1, top = 1.3, on_top = FALSE) +
  plot_annotation(
    theme = theme_minimal() +
    theme(
      plot.background = element_rect(fill = "orange4", color = NA)
    )
  )
  
