library(tidyverse)
library(here)
library(lemon)
library(RColorBrewer)
library(scales)

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

pv_ch <- park_visits %>% 
  distinct(year, unit_name, unit_type, visitors) %>% 
  filter(unit_type == "National Park" & year != "Total") %>%
  mutate(year = as.numeric(year)) %>% 
  group_by(unit_name) %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(yoy = visitors - lag(visitors)) %>% 
  filter(unit_name != "Denali National Preserve")
  
 
pv_ch %>%
  ggplot() +
  geom_line(aes(x = year, y = yoy, group = unit_name), size = 0.2) +
  scale_x_continuous(breaks = seq(1910, 2010, 10), expand = expand_scale(add = c(5, 1))) +
  scale_y_continuous(breaks = c(-3000000, 0, 1000000), labels = paste0(c(-3000000, 0, 1000000)/1000000, "M")) +
  facet_wrap(vars(unit_name), ncol = 3) +
  labs(
    title = "National Park Visits 1904–2016",
    subtitle = "Year-over-year change in total visits by park",
    caption = "Source: dataisplural/data.world | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 20, color = "grey20"),
    legend.margin = margin(0, 0, 20, 0),
    plot.background = element_rect(fill = "grey80", color = NA),
    strip.background = element_rect(fill = "grey80", color = NA),
    strip.text = element_text(family = "IBM Plex Sans Bold", color = "grey30",
                               hjust = 1, vjust = 1),
    plot.title = element_text(size = 28, color = "grey20", family = "IBM Plex Sans Medium"),
    plot.subtitle = element_text(size = 20, color = "grey20", margin = margin(5, 0, 30, 0)),
    plot.caption = element_text(size = 8, color = "grey30", margin = margin(20, 0, 0, 0)),
    axis.text.x = element_text(family = "IBM Plex Mono", size = 7, color = "grey40"),
    axis.text.y = element_text(family = "IBM Plex Mono", size = 7, color = "grey40"),
    panel.grid.major.x = element_line(color = "grey75"),
    panel.grid.major.y = element_line(color = "grey75"),
    plot.margin = margin(20, 20, 20, 20)
  ) +

# save image
  ggsave(
  here::here("week-38", "figures", "temp", paste0("national-parks", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  width = 18, height = 14, dpi = 320
)
 