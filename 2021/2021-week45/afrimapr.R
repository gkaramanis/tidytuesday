library(tidyverse)
library(camcorder)
library(afrilearndata)
library(scico)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

afripop_df <- afripop2020 %>% 
  raster::as.data.frame(xy = TRUE) %>% 
  rename(pop = 3) %>% 
  filter(!is.na(pop))

f1 = "Porpora"
f2 = "Publico Headline"

ggplot(afripop_df) +
  geom_tile(aes(x, y, fill = pop)) +
  annotate("text", -6, -5, label = "Africa", family = f2, size = 20, fontface = "bold", color = "grey97") +
  scale_fill_scico(direction = -1, trans = "pseudo_log", palette = "lajolla", breaks = c(0, 100, 1000, 10000, 20000)) +
  guides(fill = guide_colorbar(title = "Population density\n(people/km²)", label.position = "left", title.hjust = 0.5)) +
  labs(caption = "Data: afrilearndata · Graphic: Georgios Karamanis") +
  coord_fixed() +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = c(0.22, 0.23),
    legend.key.width = unit(0.5, "line"),
    legend.key.height = unit(2, "line"),
    legend.title = element_text(margin = margin(0, 0, 10, -20), color = "grey85", lineheight = 1.1),
    legend.text = element_text(color = "grey85"),
    plot.background = element_rect(fill = "#58507E", color = NA),
    plot.caption = element_text(hjust = 1, color = "grey80"),
    plot.margin = margin(10, 10, 10, 10)
  )
