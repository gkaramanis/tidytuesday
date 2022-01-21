library(tidyverse)
library(camcorder)
library(colorspace)
library(geomtextpath)

gg_record(dir = "temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

choc <- chocolate %>% 
  select(bean_origin = country_of_bean_origin, cocoa_percent, rating, review_date) %>%
  mutate(cocoa_percent = parse_number(cocoa_percent)) %>% 
  filter(bean_origin != "Blend") %>% 
  add_count(bean_origin) %>% 
  filter(n >= 50) %>% 
  count(bean_origin, rating, cocoa_percent)

f1 = "Charter"
f2 = "General Sans"
f3 = "Fira Sans"

bg_col = "#8BAACB"
col1 = colorspace::darken(bg_col, 0.4)
col2 = colorspace::lighten(bg_col, 0.5)

ggplot(choc) +
  geom_segment(data = data.frame(y = seq(20, 60, 20)), aes(x = 1, xend = 4, y = y, yend = y), color = col1, size = 0.1) +
  geom_segment(data = data.frame(x = seq(1, 4, 0.25)), aes(x = x, xend = x, y = 60, yend = 65), color = col1, size = 0.1) +
  geom_textpath(data = data.frame(bean_origin = "Belize", x = c(1, 1.5)), aes(x, 68, label = "Rating →"), text_only = TRUE, color = col1, size = 3, family = f3, vjust = 0) +
  geom_label(data = data.frame(x = 1:4), aes(x = x, y = 58, label = x), color = col1, size = 4, fill = bg_col, label.size = NA, family = f2) +
  geom_label(data = data.frame(y = seq(20, 60, 20)), aes(x = 2.5, y = y, label = y), color = col1, size = 2, fill = bg_col, label.size = NA, family = f2) +
  geom_col(aes(x = rating, y = n, fill = cocoa_percent), size = 0) +
  facet_wrap(vars(bean_origin), ncol = 5) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  scale_y_continuous(limits = c(-20, 68)) +
  scale_x_continuous(limits = c(1, 7)) +
  coord_polar(start = -pi/2, clip = "off") +
  labs(
    title = "Ratings of 728 dark chocolate bars by bean origin country",
    subtitle = str_wrap("As rated by Flavors of Cocoa between 2006 and 2021. Height of bar shows number of rated chocolate bars. Darker color represents higher cacao percentage. Showing countries with 50 or more ratings.", 100),
    caption = "Source: Flavors of Cacao · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    strip.text = element_text(family = f1, face = "bold", size = 15, margin = margin(20, 0, 5, 0), color = "darkblue"),
    panel.spacing.y = unit(-3, "lines"),
    plot.margin = margin(0, 25, -10, 25),
    plot.title = element_text(family = f2, face = "bold", size = 25, hjust = 0.5, margin = margin(20, 0, 0, 0)),
    plot.subtitle = element_text(family = f2, size = 15, hjust = 0.5, margin = margin(15, 0, 20, 0), lineheight = 1),
    plot.caption = element_text(family = f2, margin = margin(0, 0, 20, 0))
  )
