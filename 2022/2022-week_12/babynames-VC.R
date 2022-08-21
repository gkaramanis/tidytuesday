library(tidyverse)
library(camcorder)

gg_record(dir = "~/Desktop/desktop-temp", device = "png", width = 14, height = 10, units = "in", dpi = 320)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

options(scipen=999)

names_ratio <- babynames %>% 
  mutate(sex_full = case_when(
    sex == "F" ~ "female",
    sex == "M" ~ "male"
  )) %>% 
  pivot_wider(id_cols = c(year, name), values_from = n, names_from = sex_full) %>% 
  mutate(ratio = female/male) %>% 
  # keep only names for both girls and boys:
  filter(!is.na(ratio)) 

f1 <- "Porpora"
f2 <- "Produkt"

col1 <- "#ADEFD1"
col2 <- "#00203F"

ggplot(names_ratio) +
  geom_hline(yintercept = 1, size = 0.25) +
  geom_line(aes(year, ratio, group = name), size = 0.1, alpha = 0.5, color = col2) +
  # Top, more girls
  annotate("path", x = c(1925, 1925, 1947, 1947), y = c(1300, 1450, 1450, 1300), size = 0.2) +
  annotate("path", x = c(1955, 1955, 1989, 1989), y = c(1300, 1450, 1450, 1300), size = 0.2) +
  annotate("text", x = 1951, y = 2200, label = "More names given mostly to girls", size = 4, family = f1) +
  annotate("text", x = 1881, y = 1300, label = "There were more gender-neutral names that were\nmostly given to girls until the 1990s ", size = 4, family = f1, hjust = 0) +
  annotate("path", x = c(1882, 1882, 1885), y = c(700, 200, 200), size = 0.2) +
  annotate("path", x = c(1886, 1885, 1885, 1886), y = c(400, 400, 110, 110), size = 0.2) +
  # Bottom, more boys
  annotate("path", x = c(1925, 1925, 1933, 1933), y = c(0.00115, 0.001, 0.001, 0.00115), size = 0.2) +
  annotate("path", x = c(1967, 1967, 1990, 1990), y = c(0.00115, 0.001, 0.001, 0.00115), size = 0.2) +
  annotate("text", x = 1950, y = 0.0005, label = "More names given mostly to boys", size = 4, family = f1) +
  annotate("curve", x = 1936, xend = 1932, y = 0.00047, yend = 0.0008, curvature = -0.3, size = 0.1, arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("curve", x = 1964, xend = 1968, y = 0.00047, yend = 0.0008, curvature = 0.3, size = 0.1, arrow = arrow(length = unit(0.01, "npc"))) +
  # Middle, gender neutral
  annotate("text", x = c(1925, 2003), y = 1, label = c("More gender-neutral\nnames", "A lot more\ngender-neutral names"), color = "grey97", size = 4, lineheight = 0.9, family = f1) +
  scale_x_continuous(breaks = seq(1880, 2020, 10), expand = c(0.015, 0), sec.axis = dup_axis()) +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = c("100 times\nmore boys", "10 times\nmore boys", "Same number of\ngirls and boys", "10 times\nmore girls", "100 times\nmore girls")) +
  labs(
    title = "Gender-neutral names on the rise",
    subtitle = "Ratio of girls to boys for 168 000 names that were given to both genders in the U.S. from 1880 to 2017. Names that were given only to one gender are not shown.",
    caption = "Source: U.S. Social Security Administration · Graphic: Georgios Karamanis"
  ) +
  coord_cartesian() +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.background = element_rect(fill = col1, color = NA),
    plot.title = element_text(family = f2, size = 20),
    plot.subtitle = element_text(size = 12, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid = element_line(color = "#97BC62", size = 0.1)
  )
