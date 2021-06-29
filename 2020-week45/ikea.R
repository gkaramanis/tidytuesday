library(tidyverse)
library(ggimage)
library(cowplot)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

f1m <- "Futura Medium"
f1b <- "Futura Bold"

p <- ggplot(ikea) +
  geom_point(aes(width, height, alpha = depth), size = 0.5) +
  coord_fixed(ylim = c(0, 320)) +
  theme_minimal(base_family = f1m) +
  theme(
    legend.position = "none",
    axis.line = element_line(),
    panel.grid = element_blank(),
    plot.margin = margin(80, 20, 20, 20)
  )

b <- ggplot() +
  # box
  geom_rect(aes(xmin = 0, xmax = 400, ymin = 10, ymax = 150), fill = NA, color = "grey20", size = 0.75) +
  # data set
  annotate("text", x = 50, y = 130, label = "DÄTA SET", family = f1b) +
  geom_image(aes(50, 70, image = "2020-week45/img/tidytuesday.png"), asp = 2.7, size = 0.16) +
  annotate("text", x = 90, y = 100, label = "x 1", family = f1m, size = 6) +
  # ggplot
  annotate("text", x = 150, y = 130, label = "GGPLOT", family = f1b) +
  annotate("text", x = 185, y = 105, label = "x 1", family = f1m, size = 6) +
  geom_image(aes(150, 70, image = "2020-week45/img/ggplot2.png"), asp = 2.7, size = 0.14) +
  # divider
  annotate("segment", x = 210, y = 130, xend = 210, yend = 25, size = 0.75) +
  # point
  annotate("text", x = 250, y = 130, label = "PÖINT", family = f1b) +
  annotate("point", x = runif(10, 235, 265), y = runif(10, 60, 100), alpha = runif(10, 0.4, 1)) +
  annotate("text", x = 280, y = 50, label = "x 2591", family = f1m, size = 6) +
  # axis
  annotate("text", x = 340, y = 130, label = "ÄXIS", family = f1b) +
  annotate("segment", x = 345, y = 25, xend = 345, yend = 115) +
  annotate("text", x = 342, y = seq(30, 110, 20), label = seq(0, 400, 100), family = f1m, hjust = 1, size = 3) +
  annotate("text", x = 365, y = 80, label = "x 2", family = f1m, size = 6) +
  
  labs(caption = "Make sure the structure is safe. Do not leave children unattended.\nSource: Kaggle | Graphic: Georgios Karamanis") +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 10, 0),
    plot.caption = element_text(size = 7, hjust = 0, family = "Helvetica")
  )

plot_grid(p, b, ncol = 1, rel_heights = c(1, 0.55), align = "v") +
  draw_label("IKEA FURNITURE", size = 36, x = 0.05, y = 0.93, hjust = 0, fontfamily = f1b) 

ggsave(here::here("temp", paste0("ikea-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7, width = 7)

