library(ggrepel)

gg_record(dir = "temp", device = "png", width = 10, height = 14, units = "in", dpi = 320)

# ggplot(starbucks) +
#   geom_tile(aes(x = -0.1, y = caffeine_mg, width = 0.2), stat = "unique") +
#   geom_text_repel(aes(x = 0, y = caffeine_mg, label = product_name), stat = "unique", hjust = 0, direction = "y", nudge_x = 0.15, size = 3) +
#   coord_cartesian(xlim = c(-0.5, 0.5))

caff_cup <- starbucks %>% 
  filter(caffeine_mg > 0) %>% 
  mutate(name = paste0(str_to_sentence(product_name), ", ", str_to_sentence(size), ifelse(whip == 1, ", Whipped cream", ""))) %>% 
  distinct(name, caffeine_mg) %>% 
  arrange(caffeine_mg) %>% 
  mutate(
    i = row_number(),
    y = 0:(n() - 1) %/% 2,
    y2 = (y - min(y)) / max(y - min(y)) * (max(caffeine_mg) - min(caffeine_mg)) + min(caffeine_mg),
    x = rep(c(-0.5, 0.5), times = n() / 2),
    h = rep(c(1, 0), times = n() / 2)
    )

ggplot(caff_cup) +
  geom_text(aes(x = x, y = y2, label = name, hjust = h), size = 2.5) +
  geom_segment(aes(x = x, y = y2, xend = x / 2, yend = caffeine_mg), size = 0.1) + 
  coord_cartesian(xlim = c(-1, 1))
