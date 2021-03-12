library(ggrepel)

bechdel_med %>% 
  distinct(year, median_rating) %>% 
ggplot() +
  geom_text_repel(aes(x = median_rating, y = 0, label = year, color = year), position = "jitter", size = 3) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("temp", paste0("bechdel-test-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 8)

