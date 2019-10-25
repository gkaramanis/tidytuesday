library(tidyverse)
library(ggforce)

skull = tribble(
  ~skull_x, ~skull_y,
  -5, 5,
  5, 5,
  5, -3,
  2, -4,
  3.5, -7,
  -3.5, -7,
  -2, -4,
  -5, -3
)

nose = tribble(
  ~nose_x, ~nose_y,
  0, -2,
  1, -4,
  -1, -4
)

ggplot() +
  geom_bspline_closed(data = skull, aes(x = skull_x, y = skull_y)) +
  # geom_point(aes(x = skull_x, y = skull_y), color = "red") +
  geom_circle(aes(x0 = -2, y0 = -1, r = 1.5), fill = "white", color = NA) +
  geom_circle(aes(x0 = 2, y0 = -1, r = 1.5), fill = "white", color = NA) +
  geom_polygon(data = nose, aes(x = nose_x, y = nose_y), fill = "white") + 
  coord_fixed()
