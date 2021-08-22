library(tidyverse)
library(sp)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

glasses <- tribble(
  ~glass, ~v, ~r_top, ~r_bottom, ~h, ~h_top,
  "cocktail glass", 222, 11.4/2, 8.9/2, 7.1, 6.5,
  "collins glass", 360, 6.4/2, 6.4/2, 13.2, 11.2,
  "highball glass", 336, 7.6/2, 7/2, 12, 11,
  "old-fashioned glass", 303, 8.3/2, 8.3/2, 6.9, 5.6
) %>% 
  rowwise() %>% 
  mutate(
    x_inner = list(c(-r_top, r_top, r_bottom, -r_bottom, -r_top)),
    x_outer = list(x_inner * 1.01),
    y_inner = list(c(h_top, h_top, 0, 0, h_top)),
    y_outer = list(c(h_top, h_top, -(h - h_top), -(h - h_top), h_top)),
    th = atan(h / r_top - r_bottom)
  ) 

glasses_split <- glasses %>% 
  unnest(cols = c(x_inner, y_inner, x_outer, y_outer)) %>% 
  group_by(glass) %>% 
  group_split()

cocktail_p = st_polygon(list(cbind(glasses_split[[1]]$x_inner, glasses_split[[1]]$y_inner)))

collins_p = st_polygon(list(cbind(glasses_split[[2]]$x_inner, glasses_split[[2]]$y_inner)))

highball_p = st_polygon(list(cbind(glasses_split[[3]]$x_inner, glasses_split[[3]]$y_inner)))

old_p = st_polygon(list(cbind(glasses_split[[4]]$x_inner, glasses_split[[4]]$y_inner)))

ggplot() +
  geom_sf(data = cocktail_p) +
  geom_sf(data = collins_p) +
  geom_sf(data = highball_p) +
  geom_sf(data = old_p)