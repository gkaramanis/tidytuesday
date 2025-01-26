library(tidyverse)
library(patchwork)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

# Load and clean data
exped_tidy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv') %>% 
  janitor::clean_names()

# peaks_tidy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv') %>% 
  # janitor::clean_names()

# Analysis and visualization in one pipeline
expeditions <- exped_tidy %>%
  mutate(
    total_personnel = totmembers + tothired,
    success_any = success1 | success2 | success3 | success4
  ) %>%
  group_by(year, comrte) %>%
  summarise(
    "Success rate" = mean(success_any, na.rm = TRUE),
    # "Average team size" = mean(total_personnel, na.rm = TRUE),
    # "Oxygen usage rate" = mean(o2used, na.rm = TRUE),
    "Average duration (days)" = mean(totdays, na.rm = TRUE),
    expeditions = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = matches("Success|Team|Oxygen|Duration"),
    names_to = "metric",
    values_to = "value"
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- c("#8B4513", "#56B4E9")
bg_col <- "#F8F9FA"
grid_col <- "#D9D9D9"

pct_lab <- function(x) {
  ifelse(x <= 1, paste0(format(x * 100, digits = 1), "%"), x)
}

p <- ggplot(expeditions) +
  geom_line(aes(x = year, y = value, color = comrte), size = 1) +
  geom_point(aes(x = year, y = value, color = comrte, size = expeditions)) +
  scale_y_continuous(expand = expansion(mult = 0.1), labels = pct_lab) +
  facet_wrap(vars(metric), scales = "free_y", nrow = 2) +
  scale_size_area() +
  scale_color_manual(values = pal) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = bg_col, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_col),
    axis.title = element_blank(),
    strip.text = element_text(hjust = 0)
    )

q <- ggplot(expeditions) +
  geom_col(aes(x = year, y = expeditions, fill = comrte), position = position_dodge(width = 0.75), width = 0.7) +
  scale_y_continuous(breaks = seq(0, 150, 20)) +
  facet_wrap(vars("Number of expeditions per year")) +
  scale_fill_manual(values = pal) +
  labs(
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = bg_col, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_col),
    panel.grid.minor.y = element_line(color = grid_col),
    axis.title = element_blank(),
    strip.text = element_text(hjust = 0)
  )

q + p +
  plot_layout(widths = c(2, 3)) +
  plot_annotation(
    title = "<span style='color:#2A8FBF'>Commercial</span> vs <span style='color:#8B4513'>traditional</span> routes in Himalayan expeditions",
    subtitle = "Commercial routes show consistently higher success rates, reaching 96% in 2024, while maintaining shorter average durations compared\nto non-commercial expeditions. Point size indicates number of expeditions per year.",
    caption = "Source: The Himalayan Database · Graphic: Georgios Karamanis"
  ) &
  theme(
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_markdown(family = f2, face = "bold", size = 16, color = "#343A40"),
    plot.subtitle = element_text(lineheight = 1, size = 12, color = "#343A40"),
    plot.caption = element_text(size = 10, color = "#343A40"),
    plot.margin = margin(10, 10, 10, 10)
  )
