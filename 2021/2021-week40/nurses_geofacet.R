library(tidyverse)
library(ggfan)
library(geofacet)
library(scales)

nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>% 
  janitor::clean_names()

nurses_fan <- nurses %>% 
  select(state, year, annual_salary_median, contains("annual") & contains("percentile")) %>% 
  rename("percentile_0" = "annual_salary_median") %>%  
  pivot_longer(contains("perc")) %>% 
  mutate(
    percentile = parse_number(name) / 100,
    state = case_when(
      state == "District of Columbia" ~ "D.C.",
      TRUE ~ state
    )
    )

grid_ter <- us_states_territories_grid1 %>% 
  mutate(
    name = case_when(
      name == "District of Columbia" ~ "D.C.",
      TRUE ~ name
    )
  ) %>% 
  filter(code != "MP")

f1 = "October Condensed Devanagari"

bg_col = "#CFDBE3"

ggplot(nurses_fan, aes(x = year, y = value, quantile = percentile)) +
  geom_fan() +
  geom_line(aes(group = percentile), size = 0.1, color = "brown") +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(labels = label_dollar(rescale_large = rescale_short_scale())) +
  scale_fill_stepsn(colors = c("lightpink2", "mistyrose")) +
  facet_geo(vars(state), grid = grid_ter) +
  # facet_wrap(vars(state)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Annual Salaries of Registered Nurses, 1998-2020",
    subtitle = "Showing 10th, 25th, 50th (median), 75th and 90th percentile",
    caption = "Data: Data.World · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 7, color = "#01016780"),
    panel.spacing.x = unit(1, "lines"),
    strip.text = element_text(face = "bold", color = "#020271"),
    panel.grid = element_line(size = 0.35),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.26, margin = margin(20, 0, -50, 0), size = 30, face = "bold"),
    plot.subtitle = element_text(hjust = 0.19, margin = margin(60, 0, -80, 0), size = 20),
    plot.caption = element_text(hjust = 0.5)
  )

ggsave(here::here("temp", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 11)
