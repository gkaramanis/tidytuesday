library(tidyverse)
library(scales)
library(camcorder)
library(shadowtext)

gg_record(dir = "temp2", device = "png", width = 10, height = 10, units = "in", dpi = 320)

nurses_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>% 
  janitor::clean_names() 

nurses <- nurses_raw %>% 
  arrange(year) %>% 
  group_by(state) %>% 
  mutate(label = if_else(year == max(year), state, NULL)) %>% 
  ungroup()

f1 = "October Condensed Devanagari"

bg_col = "#CFDBE3"

ggplot(nurses, aes(x = total_employed_rn, y = hourly_wage_median, group = state, label = label)) +
  geom_path(aes(size = year), color = "salmon") +
  geom_point(data = nurses %>% filter(label != ""), color = "brown") +
  geom_shadowtext(aes(alpha = alpha), hjust = 0, nudge_y = 1, nudge_x = 1000, family = f1, check_overlap = TRUE, color = "#020271", size = 4, bg.colour = bg_col, bg.r = 0.08) +
  scale_size_continuous(range = c(0.3, 0.8)) +
  scale_alpha_identity() +
  scale_x_continuous(labels = label_number()) +
  scale_y_continuous(labels = label_dollar()) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Evolution of hourly wages and number of\nemployed registered nurses by state from 1998 to 2020",
    caption = "Data: Data.World · Graphic: Georgios Karamanis",
    x = "Number of employed RN",
    y = "Hourly wage"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.position = "none",
    plot.title = element_text(hjust = 0, face = "bold", size = 25),
    plot.margin = margin(20, 40, 20, 20)
  )
