library(tidyverse)
library(camcorder)
library(ggforce)

imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

scarf <- imdb %>% 
  arrange(season, ep_num) %>% 
  mutate(i = row_number())

gg_record(dir = "temp", device = "png", width = 5.5, height = 10, units = "in", dpi = 320)

fringes <- scarf %>% 
  group_by(season) %>% 
  summarise(
    xmin = min(ep_num),
    xmax = max(ep_num)
    ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(y = list(seq(-1, 1, 0.4))) %>% 
  ungroup() %>% 
  unnest(y)

bg_col = "linen"

f1 = "Aquarius"
f2 = "Doctor Who"

ggplot(scarf) +
  geom_tile(aes(x = ep_num, y = season * 4, fill = rating), width = 1, height = 2) +
  # geom_text(aes(x = ep_num, y = season * 4, label = rating), size = 1.5, color = "grey97") +
  geom_tile(data = fringes, aes(x = xmin - 1, y = season * 4 + y), height = 0.1, width = 1) +
  geom_tile(data = fringes, aes(x = xmax + 1, y = season * 4 + y), height = 0.1, width = 1) +
  geom_text(aes(x = ep_num, y = season * 4 - 1.7, label = ifelse(ep_num %% 2 == 0, ep_num, "")), family = f1, size = 3, alpha = 0.6) +
  geom_text(aes(x = -6, y = season * 4, label = paste("Season", season)), stat = "unique", family = f2, size = 7, alpha = 0.7) +
  annotate("text", x = 18, y = 9.4 * 4, label = "Source: datardis package · Graphic: Georgios Karamanis", angle = 90, family = f1, size = 3) +
  scale_fill_stepsn(colors = ghibli::ghibli_palette("TotoroMedium"), breaks = 3:10) +
  scale_y_reverse() +
  coord_fixed(clip = "off") +
  labs(title = "Doctor Who") +
  guides(fill = guide_colorsteps(title.position = "top", title = "IMDB Rating")) +
  xlim(-10, 18) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(family = f1, hjust = 0.5),
    legend.text = element_text(family = f1),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(10, 0, 10, 10),
    plot.title = element_text(size = 50, family = f2, hjust = 0.5)
  )
