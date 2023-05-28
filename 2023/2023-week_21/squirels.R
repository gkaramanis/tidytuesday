library(tidyverse)
library(cetcolor)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') %>% 
  janitor::clean_names()

p <- ggplot(squirrel_data) +
  geom_hex(aes(x, y)) +
  scale_fill_gradientn(colors = cet_pal("l8"))

hex_p <- ggplot_build(p)$data[[1]] %>% 
  rename(facet = PANEL)

f1 <- "DIN Condensed"
f2 <- "Outfit"

cp <- sf::read_sf(here::here("2019/2019-week-44/data/central-park/CentralPark.shp"))

h <- ggplot(hex_p) +
  geom_sf(data = cp, alpha = 0.1, linewidth = 0.2, color = "blue") +
  annotate("text", x = -73.987, y = 40.798, label = "2018 Central Park\nSquirrel Census", hjust = 0, size = 10, family = f1, fontface = "bold", color = "#652d08", lineheight = 0.8) +
  annotate("text", x = -73.987, y = 40.795, label = str_wrap("Center: Number of sightings in a hexagonal grid. Bottom right: Number of squirrel sightings by time of day and primary fur color.", 34), hjust = 0, vjust = 1, size = 4, family = f2, color = "#873113") +
  annotate("text", x = -73.987, y = 40.79, label = "Source: 2018 Central Park Squirrel\nCensus · Graphic: Georgios Karamanis", hjust = 0, vjust = 1, size = 3.5, family = f2, color = "#652d08", fontface = "bold") +
  geom_text(data = . %>% filter(count >= 30), aes(x, y, label = count, size = count, color = count), family = f1) +
  geom_text(data = . %>% filter(count < 30), aes(x, y, label = count, size = count, color = count), family = f1) +
  scale_color_gradientn(colors = cet_pal("l8")) +
  scale_size_continuous(range = c(1.5, 6)) +
  coord_sf(expand = FALSE, xlim = c(-73.99, -73.94), ylim = c(40.763, 40.802)) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

fc <- squirrel_data %>% 
  filter(!is.na(primary_fur_color)) %>% 
  count(primary_fur_color) %>% 
  ggplot(aes(x = primary_fur_color, y = n)) +
  geom_col(aes(fill = primary_fur_color), width = 0.7) +
  geom_text(aes(label = scales::number(n)), family = f1, nudge_y = 200, color = "#873113") +
  scale_fill_manual(values = c("black", "#D2691E", "grey")) +
  theme_void(base_family = f2) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8, color = "#652d08"))

sh <- squirrel_data %>% 
  filter(!is.na(shift)) %>% 
  count(shift) %>% 
  ggplot(aes(x = shift, y = n)) +
  geom_col(aes(fill = shift), width = 0.7) +
  geom_text(aes(label = scales::number(n)), nudge_y = 200, family = f1, color = "#873113") +
  scale_fill_manual(values = c("#00BFFF", "#000080")) +
  theme_void(base_family = f2) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8, color = "#652d08"))



h + inset_element(fc, left = 0.7, right = 0.94, bottom = 0.02, top = 0.25) +
  inset_element(sh, left = 0.8, right = 0.94, bottom = 0.3, top = 0.5)

