library(tidyverse)
library(ggridges)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# water_insecurity_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')

water_insecurity_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')

us_regions <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/refs/heads/master/us%20census%20bureau%20regions%20and%20divisions.csv") %>% 
  janitor::clean_names()



wi_23 <- water_insecurity_2023 %>% 
  mutate(state = str_split_i(name, ", ", 2)) %>% 
  
  left_join(us_regions) %>% 
  mutate(
    region = if_else(state == "Puerto Rico" | region == "South", "South and Puerto Rico", region),
    region = fct_relevel(region, "West", "Northeast", "Midwest", "South and PR")
    ) %>% 
  group_by(region) %>% 
  arrange(-percent_lacking_plumbing) %>%
  mutate(n_states = n_distinct(state)) %>% 
  ungroup() 

f1 <- "Familjen Grotesk"
f2 <- "Caladea"

ggplot(wi_23) +
  geom_text(aes(x = percent_lacking_plumbing, y = state, label = "|"), size = 2, shape = 4, alpha = 0.5, position = position_nudge(y = -wi_23$n_states/70)) +
  geom_density_ridges_gradient(aes(x = percent_lacking_plumbing, y = state, fill = stat(x)), rel_min_height = 0.01, color = "white", linewidth = 0.35, scale = 1.1) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  MetBrewer::scale_fill_met_c("Johnson", direction = -1) +
  facet_wrap(vars(region), scales = "free_y") +
  labs(
    title = "Water insecurity in the United States, 2023",
    subtitle = "Percentage of households lacking complete plumbing facilities. Each vertical line represents a county.",
    caption = "Source: US Census Bureau · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    panel.background = element_rect(fill = "grey99", color = NA),
    panel.border = element_rect(fill = NA, color = "#2F2F2F", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(family = f2, face = "bold", size = 20),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

# Alt text: A faceted ridge plot showing water insecurity across US states, grouped by regions (West, Northeast, Midwest, and South and Puerto Rico). Each state has a density curve showing the distribution of counties lacking complete plumbing facilities, with vertical lines marking individual counties. The plot reveals varying levels of water insecurity, with some states, like Arizona, Alaska, New Mexico and Puerto Rico, showing higher percentages and wider distributions than others.
