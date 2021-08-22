library(tidyverse)
library(cowplot)

datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

dtsaurus <- datasaurus %>% 
  mutate(dataset = str_to_title(str_replace(dataset, "_", " ")))

p <- ggplot(dtsaurus, aes(x, y)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  scale_x_continuous(expand = c(0.03, 0.03)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  facet_wrap(vars(dataset)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(family = "Graphik Compact Medium", size = 13, margin = margin(5, 0, 5, 0)),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey95", color = NA)
  )
  
ggdraw(p) +
  draw_label("Datasaurus Dozen", x = 0.33, y = 0.2, fontfamily = "Proxima Nova", fontface = "bold", size = 18, hjust = 0, color = "grey20") +
  draw_label("2D density estimates of the 13 datasets", x = 0.33, y = 0.165, fontfamily = "Proxima Nova", size = 16, hjust = 0, color = "grey20") +
  draw_label("Source: Alberto Cairo | Graphic: Georgios Karamanis", x = 0.33, y = 0.05, fontfamily = "Proxima Nova", size = 10, hjust = 0, color = "grey40") 

ggsave(here::here("temp", paste0("datasaurus-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 7, height = 8)

