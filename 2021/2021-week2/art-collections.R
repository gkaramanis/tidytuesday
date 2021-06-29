library(tidyverse)
library(janitor)
library(cowplot)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv') %>% 
  clean_names()

# artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv") %>% 
#   clean_names()

artwork_boxes <- artwork %>% 
  count(acquisition_year) %>% 
  mutate(
    decade = acquisition_year %/% 10 * 10,
    decade_year = acquisition_year - decade,
    size = sqrt(n)/70,
    alpha = if_else(size == max(size), 0.6, 1)
    ) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(decade_year - size * 0.475, decade_year - size * 0.475, decade_year + size * 0.475, decade_year + size * 0.475)),
    y = list(c(decade, decade - size * 9.5, decade - size * 9.5, decade))
  ) %>% 
  ungroup() %>% 
  unnest(c(x, y)) 

p <- ggplot(artwork_boxes) +
  geom_polygon(aes(x = x, y = y, group = acquisition_year, alpha = alpha), fill = "pink", color = "brown", size = 0.2) +
  annotate("text", x = 4, y = 1826, label = "In 1856, Tate\nacquired 37893\nworks of art", hjust = 1, vjust = 1, family = "Proxima Nova", size = 3, color = "brown") +
  annotate("tile", x = 4.4, y = 1827, height = 0.15, width = 0.5, color = "brown") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_reverse(breaks = seq(1820, 2010, by = 10)) +
  scale_alpha_identity() +
  coord_fixed(ratio = 0.1, expand = FALSE, clip = "off") +
  labs(caption = "Source: Tate | Graphic: Georgios Karamanis") +
  theme_void(base_family = "Produkt") +
  theme(
    plot.background = element_rect(fill = "#f3f3f6", color = NA),
    axis.text.y = element_text(margin = margin(0, 20, 0, 0), color = "grey40"),
    panel.grid.major.y = element_line(size = 0.1, color = "grey50", linetype = "dotted"),
    plot.caption = element_text(hjust = 0.5, margin = margin(30, 0, 0, 0), family = "Proxima Nova", color = "grey30"),
    plot.margin = margin(180, 35, 20, 35)
  )

artwork_legend <- data.frame(
  x = c(0.5, 1, 1.75, 2.75, 4.75),
  s = c(100, 500, 1000, 3000, 30000)
  ) %>% 
  mutate(size = sqrt(s)/70 * 0.95)

l <- ggplot(artwork_legend) +
  geom_tile(aes(x = x, y = 0 + size * 0.9 / 2, height = size * 0.9, width = size * 0.9), fill = "pink", color = "brown", size = 0.2) +
  geom_text(aes(x, -0.2, label = s, family = "Proxima Nova"), size = 2, color = "grey25") +
  coord_fixed(ratio = 1, expand = FALSE, clip = "off") +
  labs(title = "Artwork acquisitions by Tate",
       subtitle = "from 1823 to 2013") +
  theme_void(base_family = "Produkt") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, family = "Produkt Medium", color = "grey10"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(10, 0, 25, 0), color = "grey30")
    )

ggdraw(p) +
  draw_plot(l, x = 0.33, y = 0.385, width = 0.365) 

ggsave(here::here("temp", paste0("art-collections-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 4.963, height = 10)

