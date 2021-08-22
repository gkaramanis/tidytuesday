library(tidyverse)
library(geofacet)
library(colorspace)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

states_abbr <- read_csv(here::here("2020-week38", "data", "us-states-abbr-AP.csv")) %>% 
  select(state = title_case, ap_style)
  
libraries <- kids %>% 
  filter(variable == "lib") %>% 
  filter(year == 1997 | year == 2016) %>% 
  mutate(inf_adj_perchild = inf_adj_perchild * 1000) %>% 
  pivot_wider(id_cols = state, names_from = year, names_prefix = "year_", values_from = inf_adj_perchild) %>% 
  mutate(diff = year_2016 - year_1997) %>% 
  left_join(states_abbr)

f1 <- "Proxima Nova"
f1b <- "Proxima Nova Bold"
f1m <- "Proxima Nova Medium"
f2c <- "IBM Plex Sans Condensed"

ggplot(libraries) +
  # Point and arrow
  geom_point(aes(x = 0, y = year_1997, color = ifelse(diff > 0, "grey97", "grey20")), size = 1.5) +
  geom_segment(aes(x = 0, xend = 1, y = year_1997, yend = year_2016, color = ifelse(diff > 0, "grey97", "grey20")), arrow = arrow(length = unit(0.2, "cm")), size = 0.75) +
  # State label
  geom_text(aes(x = -0.3, y = 350, label = ap_style), stat = "unique", hjust = 0, color = "grey95", family = f1b, size = 3.5) +
  # 1997 value label
  geom_text(aes(x = 0, y = year_1997 - 90, label = round(year_1997)), hjust = 0.5, family = f2c, size = 3, color = darken("#7E95A9", 0.3)) +
  # 2016 value label
  geom_text(aes(x = 1.1, y = year_2016 - 90, label = round(year_2016)), hjust = 0.5, family = f2c, size = 3, color = darken("#7E95A9", 0.4)) +
  # Scales, facet, labs
  # scale_color_gradient2(low = "red", mid = "grey75", high = "grey97") +
  scale_color_identity() +
  facet_geo(vars(state)) +
  labs(
    title = "Change in public spending on libraries from 1997 to 2016",
    subtitle = "Dollars spent per child, adjusted for inflation",
    caption = "Source: Urban Institute | Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  # Theme
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#7E95A9", color = NA),
    plot.margin = margin(20, 30, 15, 30),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_blank(),
    plot.title = element_text(size = 16, family = f1b, hjust = 0.5, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 14, family = f1, hjust = 0.5, margin = margin(5, 0, 25, 0)),
    plot.caption = element_text(family = f1, hjust = 1, margin = margin(20, 0, 0, 0))
  ) 

ggsave(here::here("temp", paste0("kids-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 8)
