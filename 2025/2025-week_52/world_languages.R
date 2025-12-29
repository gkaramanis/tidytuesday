library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

endangered_status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/endangered_status.csv')
# families <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/families.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/languages.csv')

robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- rnaturalearthhires::countries10 |> 
  filter(NAME != "Antarctica") |> 
  st_transform(crs = robin)

threatened <- languages |> 
  left_join(endangered_status) |> 
  filter(!is.na(status_label)) |>
  mutate(status_label = fct_reorder(status_label, status_code)) |>
  filter(between(status_code, 2, 5)) |> 
  filter(!is.na(longitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(crs = robin) |> 
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2]
  ) 

thr_hotspots <- sfhotspot::hotspot_gistar(threatened, kde = FALSE, cell_size = 100e3)

thr_hotspots_filtered <- thr_hotspots |> 
  filter(gistar > 4)

f1 <- "Graphik Compact"
f2 <- "Charter"

# World map
guinea_bbox <- st_as_sfc(st_bbox(c(xmin = 130.4, ymin = -10.5, xmax = 152.2, ymax = 0), crs = 4326)) |> 
  st_transform(crs = robin)

p1 <- ggplot() +
  geom_sf(data = world, fill = "#E8E5DC", color = "white", linewidth = 0.2) +
  geom_sf(data = thr_hotspots_filtered, aes(alpha = gistar), color = NA, fill = "yellow") +
  geom_sf(data = threatened, aes(color = status_label), size = 0.2) +
  geom_sf(data = guinea_bbox, fill = NA, linewidth = 0.3) +
  geom_segment(aes(x = (st_bbox(guinea_bbox)["xmax"] - st_bbox(guinea_bbox)["xmin"])/2 + st_bbox(guinea_bbox)["xmin"], y = st_bbox(guinea_bbox)["ymin"], xend = (st_bbox(guinea_bbox)["xmax"] - st_bbox(guinea_bbox)["xmin"])/2 + st_bbox(guinea_bbox)["xmin"], yend = -Inf), color = "grey50", linewidth = 0.3) +
  geom_text(data = NULL, aes(x = (st_bbox(guinea_bbox)["xmax"] - st_bbox(guinea_bbox)["xmin"])/2 + st_bbox(guinea_bbox)["xmin"], y = -5.8e6, label = str_wrap("In New Guinea, the world's most linguistically diverse region, roughly 700 languages are endangered", 40)), size = 3,lineheight = 0.9, hjust = 1, vjust = 1, nudge_x = -5e5) +
  MetBrewer::scale_color_met_d("Tam") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", xlim = c(-13e6, 15e6)) +
  theme_void(base_family = f1) +
  labs(
    title = "Endangered languages",
    subtitle = str_wrap(
      "According to data from Glottolog, 1 629 of the world’s 9 138 documented languages are threatened (spoken by children only in some areas), 1 835 are shifting (spoken by all generations but losing ground), 434 are moribund (spoken only by the oldest generations), and 311 are nearly extinct. The map shows the 4 209 languages at risk (threatened, shifting, moribund, and nearly extinct), excluding nonendangered and those already extinct. Areas where many endangered languages are found together are shown in yellow.", 175),
    caption = "Data: Glottolog · Graphic: Georgios Karamanis"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 18, face = "bold", family = f2),
    plot.caption = element_text(hjust = 0, margin = margin(-10, 0, 0, 0))
  )

# Bar chart
status_n <- threatened |> 
  st_drop_geometry() |> 
  count(status_label)

p2 <- ggplot(status_n) +
  geom_col(aes(x = status_label, y = n, fill = status_label), width = 0.45) +
  geom_col(aes(x = status_label, y = n), width = 0.45, fill = "#C6BEAE", position = position_nudge(x = 0.08)) +
  geom_text(aes(x = status_label, y = -30, label = paste(str_to_sentence(status_label), scales::number(n))), hjust = 1, size = 4, fontface = "plain", color = colorspace::darken("#C6BEAE", 0.5)) + 
  MetBrewer::scale_color_met_d("Tam") +
  MetBrewer::scale_fill_met_d("Tam") +
  scale_x_discrete(limits = rev) +
  coord_flip(clip = "off", ylim = c(-500, 1900)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#E8E5DE", color = NA)
  )

# Guinea map
bbox_n <- sum(st_intersects(threatened, guinea_bbox, sparse = FALSE)) # 713 languages in the bbox

p3 <- ggplot() +
  geom_sf(data = world |> filter(SOV_A3 %in% c("IDN", "PNG")), fill = "#C6BEAE", color = "white", linewidth = 0.2) +
  geom_sf(data = threatened, aes(color = status_label), size = 0.5) +
  annotate("text", x = 133.6e5, y = -6.8e5, label = "NEW GUINEA", size = 5, hjust = 0.5) +
  annotate("text", x = 137e5, y = -8e5, label = "Papua New Guinea", size = 4, hjust = 0.5) +
  annotate("text", x = 130e5, y = -8e5, label = "Indonesia", size = 4, hjust = 0.5) +
  # geom_segment(aes(x = (st_bbox(guinea_bbox)["xmax"] - st_bbox(guinea_bbox)["xmin"])/2 + st_bbox(guinea_bbox)["xmin"] + 2.675e5, y = st_bbox(guinea_bbox)["ymax"] - 1e5, xend = (st_bbox(guinea_bbox)["xmax"] - st_bbox(guinea_bbox)["xmin"])/2 + st_bbox(guinea_bbox)["xmin"] + 2.675e5, yend = Inf), color = "grey50", linewidth = 0.2) +
  MetBrewer::scale_color_met_d("Tam") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", xlim = c(st_bbox(guinea_bbox)["xmin"], st_bbox(guinea_bbox)["xmax"]), ylim = c(st_bbox(guinea_bbox)["ymin"], st_bbox(guinea_bbox)["ymax"])) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#E8E5DE", color = NA),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  )

p23 <- p2 + p3 +
  plot_layout(widths = c(2, 1.25))

p1 / p23 +
  plot_layout(heights = c(2.6, 1)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "grey99", color = NA),
    )
  )
