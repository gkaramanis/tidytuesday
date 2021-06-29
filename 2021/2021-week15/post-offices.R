library(tidyverse)
library(here)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

decades_keep <- c(1820, 1870, 1900, 1950, 1990)

po_plot <- post_offices %>% 
  filter(established > 1000 & discontinued > 1000 & discontinued < 3000) %>% # remove wrong values
  filter(duration > 0) %>% # remove wrong values
  mutate(
    decade_est = established %/% 10 * 10,
    decade_disc = discontinued %/% 10 * 10
    ) %>% 
  filter(decade_est == 1810) %>% # keep p.o. established 1810
  group_by(decade_est, decade_disc) %>%
  summarise(n_disc = n()) %>%
  mutate(n_left = sum(n_disc) - cumsum(n_disc)) %>% 
  ungroup() %>% 
  filter(decade_disc %in% decades_keep) %>%
  rowwise() %>% 
  mutate(
    x = list(1.2 * (sqrt(n_left)/2 - ((1:n_left - 1) %% round(sqrt(n_left))))),
    y = list(pmax(0, 1.2 * (1:n_left - 1) %/% round(sqrt(n_left)))
  )) %>% # pmax 0, otherwise -Inf value for 1990
  ungroup() %>% 
  unnest(c(x, y)) %>% 
  mutate(across(x:y, ~coalesce(., 0))) %>% # replace NaN with 0
  group_by(decade_disc) %>% 
  mutate(
    dec_sign = if_else(decade_disc %in% c(1870, 1950), -1, 1),
    label_y = max(y) + 50
    ) %>% 
  ungroup()

f1b = "Source Serif Pro Bold"
f2c = "Fira Sans Condensed"

bg_col = "#E8E8E6"

ggplot(po_plot) +
  geom_point(aes(x = decade_disc + x,
                 y = (y + 15) * dec_sign,
                 shape = if_else(decade_disc == 1990, 4, 5),
                 size = if_else(decade_disc == 1990, 1, 0.25),
                 color = if_else(decade_disc == 1990, "firebrick3", "grey10")
                 )) +
  geom_text(aes(decade_disc, (label_y + 8) * dec_sign, label = paste0(decade_disc, "s")), stat = "unique", family = f1b, size = 7) +
  geom_text(aes(decade_disc, label_y * dec_sign, label = paste0(n_left, " left")), stat = "unique", family = f2c, color = "firebrick3", size = 4.5) +
  geom_linerange(aes(x = decade_disc,
                     ymin = (label_y - 10) * dec_sign,
                     ymax = (label_y - 30) * dec_sign), stat = "unique") +
  # red line
  geom_function(fun = function(x) ifelse(x > 1710, 4 * sin(0.05 * x), NA), color = "firebrick3", size = 0.75) +
  # title
  annotate("text", x = 1680, y = 85,
           label = "Post Offices\nof the 1810s",
           family = f1b, hjust = 0.5, vjust = 1, size = 17, lineheight = 0.9) +
  annotate("text", x = 1680, y = -35,
           label = str_wrap("1 625 post offices were established in the U.S. during the 1810s and, by the next decade, almost 40% of them were discontinued", 35),
           family = f2c, hjust = 0.5, vjust = 1, size = 7) +
  annotate("point", size = 0.25, shape = 5, color = "grey10",
           x = 1655 + 1.2 * ((1:1625-1) %% round(sqrt(1625))),
           y = 30 - 1.2 * ((1:1625-1) %/% round(sqrt(1625)))
           ) +
  annotate("text", x = 1598, y = -100, label = "Source: Blevins, Cameron; Helbock, Richard W., 2021, 'US Post Offices', https://doi.org/10.7910/DVN/NUKCNA ·  Graphic: Georgios Karamanis", size = 3, color = "grey50", family = f2c, hjust = 0) +
  scale_x_continuous(limits = c(1595, 2000), breaks = decades_keep) +
  scale_shape_identity() +
  scale_size_identity() +
  scale_color_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(0, 20, 0, 20)
  ) 

ggsave(here::here("temp", paste0("post-offices-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 6.5)
    
