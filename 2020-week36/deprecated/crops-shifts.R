library(tidyverse)
library(janitor)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

main_crops <- key_crop_yields %>% 
  clean_names() %>% 
  filter(!is.na(code)) %>% 
  rename_with(., ~ str_remove(.x, "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = wheat:bananas, names_to = "crop") %>% 
  group_by(entity, year) %>% 
  mutate(
    total = sum(value, na.rm = TRUE),
    pct = value/total,
    entity = str_wrap(entity, 20)
    ) %>% 
  slice_max(value, n = 1) %>% 
  ungroup() %>% 
  group_by(entity) %>% 
  mutate(
    shift = if_else(crop != lag(crop, default = NA), crop, NULL),
    n = sum(!is.na(shift))
  ) %>% 
  ungroup() %>% 
  mutate(entity = fct_reorder(entity, n, .desc = TRUE))

pal <-  c("#ffd700", "#00ffff", "#bc8f8f", "#EF6666", "#00ffff", "#006400", "#a020f0", "#1e90ff", "#ff1493")

f1 <- "DIN Condensed Bold"
f2 <- "Proxima Nova Light"

main_crops %>% 
  filter(n > 1) %>% 
ggplot() +
  annotate("tile", x = 0, y = 1989.5, height = 58, width = 1, alpha = 0.3, fill = "grey85", color = NA) +
  annotate("tile", x = 0, y = 1989.5, height = 58, width = 0.75, alpha = 0.3, fill = "grey75", color = NA) +
  annotate("tile", x = 0, y = 1989.5, height = 58, width = 0.5, alpha = 0.3, fill = "grey80", color = NA) +
  geom_tile(aes(x = 0, y = year, width = pct, fill = crop), color = NA) +
  scale_y_reverse() +
  scale_fill_manual(values = pal, guide = guide_legend(ncol = 1, keyheight = 0.5)) +
  coord_cartesian(expand = FALSE) +
  facet_wrap(vars(entity), ncol = 20, strip.position = "left") +
  theme_void() +
  theme(
    legend.text = element_text(family = f2),
    strip.text = element_text(family = f1, size = 9, hjust = 0, lineheight = 0.8),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("temp", paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 10)
