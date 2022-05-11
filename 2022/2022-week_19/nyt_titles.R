library(tidyverse)
library(ggsvg)
library(shadowtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

decade_books <- nyt_full %>% 
  mutate(decade = (year - 1) %/% 10 * 10) %>% 
  distinct(decade, title_id) %>%
  count(decade)

books_draw <- decade_books %>% 
  rowwise() %>% 
  mutate(
    y = list(1:(n/20)),
    ymax = max(y)
    ) %>% 
  ungroup() %>% 
  unnest(y) 

book_svg <- paste(readLines(here::here("2022/2022-week_19/svg/book.svg")), collapse = "\n")

f1 <- "Borsok"
f2 <- "Porpora"

ggplot(books_draw) +
  geom_point_svg(aes(x = decade, y = y), svg = book_svg, size = 15) +
  geom_shadowtext(aes(x = decade, y = ymax + 0.5, label = paste0(n, "\n▼")), stat = "unique", family = f1, size = 6, color = "white", vjust = 0, lineheight = 0.95) +
  scale_x_continuous(breaks = seq(1930, 2010, 10), labels = paste0(seq(1930, 2010, 10), "s")) +
  ylim(0, 99) +
  labs(
    title = "Number of books featured in\nThe New York Times Hardcover Fiction Bestsellers",
    subtitle = "1931-2020, by decade",
    caption = "Source: Post45 Data Collective · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2) +
  coord_cartesian(clip = "off") +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.text.x = element_text(family = f2, size = 20, color = "black"),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 20, margin = margin(7, 0, -10, 0)),
    plot.margin = margin(15, 15, 10, 15),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
  

