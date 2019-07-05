library(tidyverse)
library(here)
library(cowplot)
library(RColorBrewer)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

sorted_rev <- media_franchises %>%
  group_by(franchise) %>%
  mutate(revenue_perc = round(revenue/sum(revenue)*100, 1)) %>% 
  select(franchise, revenue_category, revenue, revenue_perc) %>% 
  arrange(franchise, -revenue_perc) %>%
  mutate(order = row_number())

# saved me:
# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

# main plot
p <- sorted_rev %>%
  ungroup() %>% 
  mutate(franchise = str_replace(franchise, " / ", "\n")) %>% 
ggplot() +
  geom_col(aes(x = order,
               y = revenue_perc,
               fill = revenue_category),
           # width affects only the biggest revenue, gives warnings
           width = 3
           ) +
  facet_wrap(~franchise, ncol = 10) +
  scale_x_reverse() +
  scale_fill_brewer(palette = "Dark2") +
  coord_polar(theta = "y") +
  labs(
    title = "Media Franchise Powerhouses",
    subtitle = "Different revenue streams as percentage of the total revenue.\nThe outer ring shows the largest revenue stream (full circle is 100%)",
    caption = "Source: Wikipedia | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff5ba", color = "#fff5ba"),
    plot.margin = margin(0, 50, 0, 50),
    strip.text.x = element_text(size = 1),
    plot.title = element_text(family = "Space Mono Bold",
                              size = 11, hjust = 0.5,
                              margin = margin(30, 0, 5, 0)),
    plot.subtitle = element_text(family = "Space Mono",
                              size = 7, hjust = 0.5,
                              margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(family = "Space Mono", 
                                size = 6, hjust = 0.5,
                                margin = margin(30, 0, 30, 0)),                      
    text = element_text(family = "Space Mono")
  )

# custom "legend" plot
l <- tribble(
  ~category, ~x, ~y,
  "Video Games/Games", 1.5,  2,
  "Box Office", 2.15,  2,
  "Home Video/Entertainment", 3.65, 2,
  "Music", 4, 2,
  "TV", 0.5, 1,
  "Book Sales", 1.15, 1,              
  "Merchandise, Licensing & Retail", 3.1, 1,
  "Comic or Manga", 4, 1
  ) %>% 
  ggplot(aes(label = category, x = x, y = y,
             color = category)) +
  # geom_text(family = "Space Mono Bold",
  #           hjust = 1, size = 1.5) +
  geom_label(aes(fill = category),
             label.r = unit(0, "lines"),
             label.padding = unit(0.05, "lines"),
             color = "#fff5ba",
             family = "Space Mono Bold",
             hjust = 1, size = 1.5) +
  scale_fill_brewer(palette = "Dark2") +
  coord_fixed(ratio = 0.2, xlim = c(0, 4), ylim = c(0, 2.5)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff5ba", color = "#fff5ba")
  )

# plot and save
ggdraw() +
  draw_plot(p) +
  draw_plot(l, x = 0.35, y = 0.11, width = 0.52, height = 0.1) +
  ggsave(here("week-27", "media_franchises_circles.png"),
    height = 6.35, width = 5, dpi = 900)

