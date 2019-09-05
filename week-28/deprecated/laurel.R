library(tidyverse)
library(ggforce)

laurel <- data.frame(
  start = c(0.7, -0.7),
  end = c(pi - 0.1, -pi + 0.1),
  r = c(0.7, 0.7)
  )

leaf <- data.frame(
  x = c(0.6, 0.5, 0.8, 0.5, 0.6, 0.3),
  y = c(1, 1, 0.5, 0, 0, 0.5)
  )

ggplot() +
  geom_arc(data = laurel, aes(x0 = 0, y0 = 0, r = r, start = start, end = end)) +
  geom_bspline_closed(data = leaf, aes(x, y), alpha = 0.5) +
  coord_fixed() +
  
  ggsave(here::here("week-33", "img_plot", paste0("emperors", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         dpi = 320)
