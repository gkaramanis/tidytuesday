library(tidyverse)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

world <- rnaturalearthdata::countries110 %>% 
  select(country_code = adm0_a3, continent)

world_democracy <- democracy_data %>% 
  left_join(world) %>% 
  count(year, regime_category_index) %>% 
  filter(!is.na(regime_category_index)) 

p <- ggplot(world_democracy) +
  geom_col(aes(year, n, fill = factor(regime_category_index)), color = "black", linewidth = 0.1) 
  
regime_categories <- tribble(
  ~regime_category_index, ~regime_type,      ~regime_category, ~color,
  0,                     "Parliamentary",     "Democracy",      "#2B5D8C", # Darker blue
  1,                     "Semi-Presidential", "Democracy",      "#4B86B4", # Mid blue
  2,                     "Presidential",      "Democracy",      "#85B4D6", # Light blue
  3,                     "Civilian",         "Dictatorship",   "#A6324C", # Darker burgundy
  4,                     "Military",         "Dictatorship",   "#D95F69", # Mid red
  5,                     "Royal",            "Dictatorship",   "#F59B9B"  # Light salmon
) %>% 
  mutate(group = regime_category_index + 1)

dd <- layer_data(p) %>%
  left_join(regime_categories) %>% 
  group_by(x) %>% 
  reframe(
    x,
    group = factor(group),
    regime_type,
    regime_category,
    y1 = min(ymin),
    y2 = max(ymax),
    ymin = ymin - (y2 - y1) / 2,
    ymax = ymax - (y2 - y1) / 2,
    color
  )
  
f1 <- "Graphik Compact"

ggplot(dd) +
  annotate("segment", x = c(1950, 2020), xend = c(1950, 2020), y = 0, yend = c(88, 103), color = "black", linetype = "dotted", linewidth = 0.7) +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = ymin, ymax = ymax, fill = color), color = "black", linewidth = 0.15) +
  annotate("segment", x = 1949.5, xend = 2020.5, y = 0, color = "white", alpha = 0.5) +
  annotate("richtext", x = 1949.2, y = 100, label = "**1950**<br>39 democracies<br>52 dictatorships", family = f1b, hjust = 0, label.size = 0, fill = alpha("white", 0.5)) +
  annotate("richtext", x = 2020.7, y = 115, label = "**2020**<br>116 democracies<br>76 dictatorships", family = f1b, hjust = 1, label.size = 0, fill = alpha("white", 0.5)) +
  scale_fill_identity(guide = guide_legend(byrow = TRUE, nrow = 2), labels = paste(regime_categories$regime_type, regime_categories$regime_category)) +
  labs(
    title = "Seven decades of political change: democracy becomes the dominant system",
    subtitle = "Number of countries by type of government, 1950-2020",
    caption = "Source: The Democracy and Dictatorship Dataset · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(hjust = 0.5, size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )
  

