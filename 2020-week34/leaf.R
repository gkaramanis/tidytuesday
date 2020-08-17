library(ggforce)

leaf_up <- data.frame(
  x = c(0, -1.5, -1.5, 0, 0, 1.5, 1.5, 0),
  y = c(0.5, 4.5, 9, 14, 14, 9, 4.5, 0.5)
) %>% 
  mutate(a = list(c(-40, -20, 0, 20, 40))) %>% 
  unnest(a) %>% 
  mutate(
    x = x * cos(a * pi / 180) - y * sin(a * pi / 180),
    y = y * cos(a * pi / 180) + x * sin(a * pi / 180)
  )

leaf_down <- data.frame(
  x = c(0, -1, -1, 0, 0, 1, 1, 0),
  y = c(1.5, 5.5, 9.5, 13.5, 13.5, 9.5, 5.5, 1.5)
) %>% 
  mutate(a = list(seq(130, 230, length.out = 11))) %>% 
  unnest(a) %>% 
  mutate(
    x = x * cos(a * pi / 180) - y * sin(a * pi / 180),
    y = y * cos(a * pi / 180) - x * sin(a * pi / 180)
  )

ggplot() +
  geom_bspline_closed(data = leaf_up, aes(x, y, group = a), alpha = 0.7) +
  geom_polygon(data = leaf_up, aes(x, y, group = a), alpha = 0.7, color = "firebrick") +
  geom_bspline_closed(data = leaf_down, aes(x, y, group = a), alpha = 0.7) +
  geom_polygon(data = leaf_down, aes(x, y, group = a), alpha = 0.7, color = "firebrick") +
  coord_fixed() +
  ggsave(here::here("temp", paste0("plants-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
