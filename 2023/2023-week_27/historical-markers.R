library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')

us <- rgeoboundaries::gb_adm1("usa") %>% 
  st_transform(st_crs(2163)) %>% 
  rmapshaper::ms_simplify()

markers_decade <- historical_markers %>% 
  mutate(decade = year_erected %/% 10 * 10) %>% 
  filter(!is.na(decade)) %>% 
  mutate(decade = if_else(decade < 1900, "1850s-1890s", paste0(as.character(decade), "s")))

decades <- sort(unique(markers_decade$decade)) 

state_decade <- markers_decade %>% 
  group_by(decade) %>% 
  count(state_or_prov, sort = TRUE) %>% 
  slice_max(order_by = n, n = 3) %>% 
  mutate(
    y = rev(row_number()),
    state_or_prov = if_else(y == 3, paste0("**", state_or_prov, "**"), state_or_prov),
    nc = as.character(scales::number(n)),
    nc = if_else(y == 3, paste0("**", nc, "**"), nc)
    ) %>% 
  ungroup()

markers_density <- ggplot(markers_decade) +
  ggpointdensity::geom_pointdensity(aes(x = longitude_minus_w, y = latitude_minus_s), size = 0.25) +
  facet_wrap(vars(decade))

markers_proj <- ggplot_build(markers_density)$data[[1]] %>% 
  st_as_sf(crs = 4326, coords = c("x", "y")) %>% 
  st_transform(st_crs(2163)) %>% 
  rename(facet = PANEL) %>% 
  mutate(decade = decades[facet])

f1 <- "Outfit"
f2 <- "Source Serif Pro"

pal <- RColorBrewer::brewer.pal(9, "YlOrRd")[4:9]

p <- ggplot(markers_proj) +
  geom_sf(data = us, fill = "#F5FEFC", linewidth = 0.1) +
  geom_sf(aes(color = n_neighbors), size = 0.4) +
  ggtext::geom_richtext(data = state_decade, aes(x = -1.95e6, y = -2.3e6 + y * 2e5, label = paste(state_or_prov, nc)), hjust = 0, size = 2.5, family = f1, fill = NA, label.color = NA, label.padding = unit(0, "lines")) +
  scale_color_gradientn(colors = pal) +
  facet_wrap(vars(decade)) +
  coord_sf(xlim = c(-2e6, 2.5e6), ylim = c(-2.3e6, 0.73e6)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(family = f2, face = "bold")
  )

t <- ggplot() +
  geom_text(aes(0, 0, label = "U.S. Historical Markers"), family = f2, size = 9.5, hjust = 0, fontface = "bold") +
  geom_text(aes(0, -0.55, label = "Location of new historical markers in The Historical Marker\nDatabase by decade erected. The 3 states with the most\nnew markers are shown for each decade."), family = f2, size = 4, hjust = 0, lineheight = 0.9) +
  geom_text(aes(0, -1, label = "Source: The Historical Marker Database · Graphic: Georgios Karamanis"), family = f1, size = 3, hjust = 0) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(-1, 0.5)) +
  coord_cartesian(clip = "off") +
  theme_void()

p +
  inset_element(t, left = 0.5, right = 0.9, bottom = 0.08, top = 0.28)

