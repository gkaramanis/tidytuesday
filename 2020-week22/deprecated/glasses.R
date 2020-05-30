glasses <- tribble(
  ~glass, ~v, ~d_top, ~d_bottom, ~h, ~h_top,
  "cocktail glass", 222, 11.4, 8.9, 7.1, 6.5,
  "collins glass", 360, 6.4, 6.4, 13.2, 11.2,
  "highball glass", 336, 7.6, 7, 12, 11,
  "old-fashioned glass", 303, 8.3, 8.3, 6.9, 5.6
) %>% 
  rowwise() %>% 
  mutate(
    r_top = d_top / 2,
    r_bottom = d_bottom / 2,
    x_inner = list(c(-r_top, r_top, r_bottom, -r_bottom)),
    x_outer = list(x_inner * 1.01),
    y_inner = list(c(h_top, h_top, 0, 0)),
    y_outer = list(c(h_top, h_top, -(h - h_top), -(h - h_top)))
  )

dark_plot <- dark_rum %>% 
  left_join(glasses) %>%  
  unnest(c(x_inner, x_outer, y_inner, y_outer))

ggplot(dark_plot) +
  geom_polygon(aes(x = x_outer, y = y_outer, group = glass), fill = "#a8ccd7", colour = "#a8ccd7", size = 0.5) +
  geom_polygon(aes(x = x_inner, y = y_inner, group = glass), fill = "white", colour = NA) +
  coord_fixed() +
  theme_void() +
  facet_wrap(vars(drink))

