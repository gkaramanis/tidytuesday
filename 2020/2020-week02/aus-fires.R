library(tidyverse)
library(here)
source("theme-geo.R")

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')

rf_m <- rainfall %>% 
  na.omit() %>% 
  filter(year < 2020) %>% 
  group_by(city_name, year, month) %>% 
  summarise(mean_rainfall = mean(rainfall/period))

ggplot(rf_m) +
  geom_vline(xintercept = 2019, size = 0.2, color = "orange", alpha = 0.5, linetype = "dotted") +
  geom_linerange(aes(x = year, ymin = 0, ymax = mean_rainfall), color = "aliceblue",  alpha = 0.2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1850, 2020, 20), labels = c("", seq(1870, 2020, 20))) +
  scale_y_reverse(expand = c(0.0025, 0)) +
  facet_wrap(vars(city_name), ncol = 3) +
  labs(
    title = "Mean monthly rainfall, overlaid by year, from 1858 to 2019 ",
    caption = "Data: Bureau of Meteorology | Graphic: Georgios Karamanis",
    y = "Rainfall in millimeters"
  ) +
  theme_linedraw() +
  theme_geo +
  theme(
    panel.background = element_rect(fill = "#315053", color = "black"),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "black", color = "black"),
    strip.text = element_text(family = "IBM Plex Sans Thin", size = 16),
    panel.grid.major = element_line(color = "grey20", size = 0.1),
    panel.grid.minor = element_line(color = "grey25", size = 0.07),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 7, color = "grey30"),
    axis.title.y = element_text(family = "IBM Plex Sans", size = 12),
    plot.title = element_text(family = "IBM Plex Sans Medium"),
    plot.caption = element_text(color = "grey40")
  ) 

ggsave(
    here::here("2020-week02", "plots", "temp", paste0("aus-fires-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 12
  )

