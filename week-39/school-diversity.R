library(tidyverse)
library(janitor)
library(here)
library(viridis)

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

clean_school <- school_diversity %>% 
  clean_names() %>% 
  group_by(st, school_year) %>% 
  mutate(
    aian = median(aian),
    asian = median(asian),
    black = median(black),
    hispanic = median(hispanic),
    white = median(white),
    multi = median(multi)
  ) %>% 
  ungroup() %>% 
  gather("bg", "pct", aian:multi) %>%
  mutate(
    year = as.numeric(factor(school_year)),
    bg = paste0(bg, "_", year)
         ) %>% 
  distinct(st, bg, pct) %>% 
  spread(bg, pct)


ggplot(clean_school) +
  # "axes" and "gridlines"
  annotate(
    "segment",
    x = seq(0, 100, 10), y = 19, 
    xend = seq(0, 100, 10), yend = 3.5,
    size = 0.2,
    color = "grey90",
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = seq(0, 100, 20), y = 3.2, 
    label = paste0(seq(0, 100, 20), "%"),
    family = "IBM Plex Sans",
    size = 3.5,
    color = "grey55",
    alpha = 0.9
  ) +
  # grey bars and bar labels
  annotate(
    "rect",
    xmin = 0,
    ymin = seq(20, 5, -3),
    xmax = 100,
    ymax = seq(19, 4, -3),
    color = "#cdd4db",
    fill = "#cdd4db"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = seq(20.5, 5.5, -3),
    label = c("american indian and alaskan native", "asian", "black", "hispanic", "white", "multiracial"),
    family = "IBM Plex Sans Bold",
    hjust = 0,
    size = 4.5,
    alpha = 0.7
  ) +
  # 1 - aian
  geom_segment(aes(
    x = aian_1,
    y = 20,
    xend = aian_2,
    yend =  19,
    color = st
  ),
  size = 0.2) +
  geom_text(
    data = subset(clean_school, aian_2 > 3),
    aes(x = aian_2, y = 18.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # 2 - asian
  geom_segment(aes(
    x = asian_1,
    y = 17,
    xend = asian_2,
    yend =  16,
    color = st
  ), size = 0.2) +
  geom_text(
    data = subset(clean_school, asian_2 > 50),
    aes(x = asian_2, y = 15.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # 3 - black
  geom_segment(aes(
    x = black_1,
    y = 14,
    xend = black_2,
    yend =  13,
    color = st
  ), size = 0.2) +
  geom_text(
    data = subset(clean_school, black_2 > 5),
    aes(x = black_2, y = 12.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # 4 - hispanic
  geom_segment(aes(
    x = hispanic_1,
    y = 11,
    xend = hispanic_2,
    yend =  10,
    color = st
  ), size = 0.2) +
  geom_text(
    data = subset(clean_school, hispanic_2 > 19),
    aes(x = hispanic_2, y = 9.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # 5 - white
  geom_segment(aes(
    x = white_1,
    y = 8,
    xend = white_2,
    yend =  7,
    color = st
  ), size = 0.2) +
  geom_text(
    data = subset(clean_school, white_2 < 39),
    aes(x = white_2, y = 6.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # 6 - multi
  geom_segment(aes(
    x = multi_2,
    y = 4.1,
    xend = multi_2,
    yend =  4,
    color = st
  ), size = 0.2) +
  geom_text(
    data = subset(clean_school, multi_2 > 13),
    aes(x = multi_2, y = 3.7, label = st, color = st),
    family = "IBM Plex Sans",
    size = 3.5,
    alpha = 0.7
  ) +
  # plot title, scales and theme
  labs(
    title = "Changes in proportion of students' racial groups from the 1994-1995 to the 2016-2017 school year",
    subtitle = "Showing change in median value by state. Multiracial was not a category in 1994-1995",
    caption = "Source: The Washington Post | Graphic: Georgios Karamanis"
  ) +
  scale_color_viridis_d(option = "inferno") +
  coord_fixed(ratio = 4) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "none",
    # plot.background = element_rect(fill = "#fbfcfc"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 1, family = "IBM Plex Sans Bold", color = "grey25"),
    plot.subtitle = element_text(hjust = 1, margin = margin(10, 0, 20, 0), color = "grey25")
  ) +
  ggsave(
    here::here("week-39", "figures", "temp", paste0("school-diversity", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    width = 12, height = 9, dpi = 320
    )
