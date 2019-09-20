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
  mutate(pct_change = (visitors/lag(visitors) - 1) * 100) %>% 
  filter(unit_name != "Denali National Preserve") %>% 
  drop_na() %>% 
  mutate(
    pct_change = case_when(
      pct_change > 100 ~ 200,
      pct_change < -100 ~ -200,
      TRUE ~ pct_change
    )
  )
  
 
pv_ch %>%
  ggplot() +
  geom_segment(aes(x = year, xend = year,
                   y = 0, yend = 1, color = pct_change), size = 1) +
  scale_x_continuous(breaks = seq(1910, 2010, 20), expand = expand_scale(add = c(5, 1))) +
  scale_y_continuous(expand = c(0.05, 0.25)) +
  facet_wrap(vars(unit_name), ncol = 3) +
  scale_colour_gradientn(limits = c(-200, 200),
                         colors = brewer.pal(n = 7, name = "RdYlGn"),
                         values = rescale(c(-200, -100, -1, 0, 1, 100, 200)),
                         labels = c("", "-100%", "0", "100%", "")
                         ) +
  labs(
    title = "National Park Visits 1904–2016",
    subtitle = "Year-over-year percentage change",
    caption = "Source: dataisplural/data.world | Graphic: Georgios Karamanis"
  ) +
  guides(color = guide_colorbar(
    title.position = "top",
    label.position = "top",
    title = NULL,
    barwidth = 20,
    barheight = 0.5
    )) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 20, color = "grey20"),
    legend.margin = margin(0, 0, 20, 0),
    plot.background = element_rect(fill = "grey70", color = NA),
    strip.background = element_rect(fill = "grey70", color = NA),
    strip.text = element_text(family = "IBM Plex Sans Bold", color = "grey30",
                               hjust = 1, vjust = 1),
    plot.title = element_text(size = 28, color = "grey20", family = "IBM Plex Sans Medium"),
    plot.subtitle = element_text(size = 20, color = "grey20"),
    plot.caption = element_text(size = 8, color = "grey30", margin = margin(20, 0, 0, 0)),
    axis.text.x = element_text(family = "IBM Plex Mono", size = 7, color = "grey40"),
    panel.grid.major.x = element_line(color = "grey65"),
    plot.margin = margin(20, 20, 20, 20)
  ) +

# save image
  ggsave(
  here::here("week-38", "figures", "temp", paste0("national-parks", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  width = 18, height = 14, dpi = 320
)
 