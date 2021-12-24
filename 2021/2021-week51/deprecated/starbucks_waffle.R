library(tidyverse)
library(camcorder)
library(waffle)
library(MetBrewer)

gg_record(dir = "temp", device = "png", width = 14, height = 8, units = "in", dpi = 320)

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

caff <- starbucks %>% 
  distinct(product_name, caffeine_mg) %>%
  filter(caffeine_mg > 0) %>% 
  group_by(product_name) %>% 
  arrange(caffeine_mg) %>% 
  mutate(i = row_number()) %>% 
  mutate(
    max_caff = max(caffeine_mg),
    diff_caff = caffeine_mg - lag(caffeine_mg),
    diff_caff = ifelse(is.na(diff_caff), caffeine_mg, diff_caff)
    ) %>% 
  ungroup() %>% 
  mutate(product_name = fct_reorder(product_name, -max_caff)) %>% 
  pivot_longer(cols = c(diff_caff)) 

f1 = "Porpora"

pal <- rev(met.brewer("OKeeffe2"))
col_bg <- met.brewer("Veronese")[6]

ggplot(caff) +
  geom_vline(xintercept = 400 / 5) +
  geom_waffle(aes(fill = factor(i), values = value), color = col_bg, size = 0.2, n_rows = 5, flip = FALSE) +
  scale_x_continuous(breaks = seq(100/5, 500/5, by = 100/5), labels = function(x) x * 5, expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  coord_fixed() +
  facet_wrap(~ product_name, ncol = 5, strip.position = "bottom") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_bg, color = NA),
    axis.text.x = element_text(color = "grey97"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 8, hjust = 0, color = "grey98")
  )
