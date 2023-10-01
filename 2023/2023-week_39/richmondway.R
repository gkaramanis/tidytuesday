library(tidyverse)
library(shadowtext)
library(ggimage)
library(cropcircles)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 7, height = 8, units = "in", dpi = 320)

richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv') %>% 
  janitor::clean_names()

f1 <- "Outfit"
f2 <- "Bricolage Grotesque 96pt Condensed"

col_blue <- "#0176F2"
col_red <- "#EA0406"
col_grey <- "#493B4B"
col_grey_l <- colorspace::lighten(col_grey, 0.2)

ggplot(richmondway) +
  # Fuck! count
  geom_text(aes(season, episode, label = f_count_rk, size = f_count_rk, color = f_count_rk), family = f2, fontface = "bold", color = col_grey, alpha = 0.1) +
  scale_size_continuous(range = c(10, 22)) +
  ggnewscale::new_scale("size") +
  # Fuck! text
  geom_text(aes(season, episode, label = "FUCK!", size = f_count_rk, color = f_count_rk), family = f2, fontface = "bold") +
  # Title
  geom_image(aes(-0.5, 2.52, image = circle_crop(here::here("2023/2023-week_39/img/rk_halftone.png"))), size = 0.19) +
  annotate("shadowtext", -0.5, 1, label = "ROY KENT\n\n\nFUCKS PER\nEPISODE", hjust = 0.5, vjust = 1, family = f2, size = 12, fontface = "bold", lineheight = 0.9, color = col_grey_l, bg.r = 0.04, bg.color = "grey99") +
  annotate("text", -0.5, 5.4, label = "✹\n\nSource:\nrichmondway\nR package\n\nGraphic:\n Georgios Karamanis", hjust = 0.5, vjust = 1, family = f2, size = 5, lineheight = 0.9, color = col_grey) +
  scale_color_steps(low = col_blue, high = col_red) +
  scale_size_continuous(range = c(3, 13)) +
  scale_x_continuous(breaks = 1:3, position = "top", expand = c(0.12, 0.12)) +
  scale_y_reverse(breaks = 1:12) +
  labs(
    x = "SEASON",
    y = "EPISODE"
  ) +
  coord_fixed(clip = "off", ratio = 0.45, xlim = c(1, 3)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey97", color = col_grey, linewidth = 0.1),
    axis.text = element_text(color = col_grey_l, size = 9, margin = margin(0, 2, 2, 0), face = "bold"),
    axis.title = element_text(margin = margin(5, 5, 5, 5), size = 9, color = col_grey_l, face = "bold"),
    axis.title.y = element_text(angle = 90),
    plot.margin = margin(10, 10, 10, 150)
  )
