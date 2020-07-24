library(tidyverse)
library(ggforce)
library(futurevisions)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

a = 5
b = 6

hrs_sum <- astronauts %>% 
  distinct(name, total_hrs_sum, total_eva_hrs) %>% 
  slice_max(total_hrs_sum, n = 20) %>% 
  mutate(
    years = total_hrs_sum / 24 / 30.25 / 12,
    t = round(2 * pi * years, 1)
    ) %>% 
  rowwise() %>% 
  mutate(
    x = (a + b * t) * cos(t),
    y = (a + b * t) * sin(t),
    label_x = (20 + a + b * t) * cos(t),
    label_y = (20 + a + b * t) * sin(t)
  ) %>% 
  ungroup() %>% 
  group_by(t) %>% 
  mutate(
    label_name = paste(name, collapse = "\n"),
    label_x = case_when(
      x < 0 ~ min(label_x),
      TRUE ~ label_x
    ),
    label_y = case_when(
      x < 0 ~ max(label_y),
      TRUE ~ label_y
      )
    ) %>% 
  ungroup() %>% 
  mutate(
    hjust = if_else(x > 0, 0, 1),
    angle = round(atan2(y, x), 1),
    angle = case_when(
      x < 0 ~ angle + pi,
      TRUE ~ angle
    )
  ) 


max_year = max(hrs_sum$years)

orbit <- data.frame(
  t = seq(0, 2 * pi * max_year, by = 0.1)
) %>% 
  rowwise() %>% 
  mutate(
    x = (a + b * t) * cos(t),
    y = (a + b * t) * sin(t)
  ) 

source(here::here("2020-week29", "astronauts-spiro-bg.R"))

pal <- futurevisions("kepler16b")

ggplot(hrs_sum) +
  geom_raster(data = df, aes(x, y, fill = zColor)) +
  annotate("point", -1, -2, size = 16, colour = "#478AE3") +
  geom_bspline(data = orbit, aes(x, y), n = 400) +
  geom_point(aes(x, y), colour = pal[2]) +
  geom_segment(aes(x = x * 1.05, y = y * 1.05, xend = 0.97 * label_x, yend = 0.97 * label_y), size = 0.3, colour = pal[1]) +
  geom_text(aes(label_x, label_y, label = label_name, angle = angle * 180/pi, hjust = hjust),
            size = 3.5, check_overlap = TRUE, family = "DIN Condensed Bold", lineheight = 0.7, colour = pal[7]) +
  scale_size(range = c(0.2, 2.5)) +
  scale_colour_gradient(low = pal[7], high = pal[6]) +
  scale_fill_gradient(low = pal[4], high = pal[1]) +
  coord_fixed(clip = "off") +
  xlim(-190, 190) +
  ylim(-190, 190) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(50, 50, 50, 50)
  ) +
  ggsave(here::here("2020-week29", "plots", "temp", paste0("astronauts-spiro", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 10)

