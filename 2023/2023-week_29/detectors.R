library(tidyverse)
library(ggridges)

library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv') %>% 
  janitor::clean_names() %>% 
  filter(!is.na(native))

ggplot(detectors, aes(x = pred_ai, y = native, fill = native)) + 
  geom_density_ridges(color = "grey30", scale = 0.9) +
  geom_text(aes(label = "|", color = native), nudge_y = -0.2) +
  scale_fill_manual(breaks = c("Yes", "No"), values = c("#14133B", "#C59CC7"), name = "Native English writer") +
  scale_color_manual(breaks = c("Yes", "No"), values = c("#14133B", "#C59CC7"), name = "Native English writer") +
  facet_wrap(vars(detector), ncol = 2) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Predicted probability that sample was written by AI", 
    caption = "Source: detectors R package · Graphic: Georgios Karamanis") +
  theme_minimal(base_family = "Outfit", base_size = 12) +
  theme(
    legend.position = c(0.8, 0.12), 
    plot.background = element_rect(fill = "grey99", color = NA), 
    axis.title = element_blank(), 
    strip.text = element_text(size = 12), 
    plot.title = element_text(face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
    )
