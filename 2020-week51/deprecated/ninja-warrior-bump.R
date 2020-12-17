library(ggbump)

ninja_warrior %>% 
  distinct(season, obstacle_name, obstacle_order) %>% 
  ggplot() +
  geom_bump(aes(season, obstacle_order, group = obstacle_name, color = obstacle_name), alpha = 0.5, size = 2) +
  geom_point(aes(season, obstacle_order, color = obstacle_name), size = 4, alpha = 0.5) +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("temp", paste0("ninja-warrior-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 9)

