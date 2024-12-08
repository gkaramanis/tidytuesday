library(tidyverse)
library(camcorder)
library(lubridate)

gg_record(dir = "tidytuesday-temp", device = "png", width = 13, height = 8, units = "in", dpi = 320)

A64_traffic <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv') %>% 
  janitor::clean_names()

a64_avg <- A64_traffic %>% 
  select(name, report_date, time_period_ending, avg_mph, total_volume) %>%
  mutate(
    time_of_day = case_when(
      between(hour(time_period_ending), 18, 23) | between(hour(time_period_ending), 0, 5) ~ "night",
      TRUE ~ "day"
    )
  ) %>%
  group_by(name, time_of_day) %>%
  mutate(
    # Rescale hours within each group to span 0-2π
    hour_val = case_when(
      time_of_day == "day" ~ (hour(time_period_ending) - 6) / 12,  # 6-18 mapped to 0-1
      time_of_day == "night" & hour(time_period_ending) >= 18 ~ (hour(time_period_ending) - 18) / 12,  # 18-24 mapped to 0-0.5
      time_of_day == "night" ~ (hour(time_period_ending) + 6) / 12  # 0-6 mapped to 0.5-1
    ),
    angle = hour_val * 2 * pi - (pi + if_else(time_of_day == "day", -pi/12, pi/12)),
    x = cos(angle) * avg_mph,
    y = sin(angle) * avg_mph
  ) %>%
  ungroup()

# Create reference circles data
ref_circles <- expand.grid(
  speed = c(30, 60),
  angle = seq(0, 2*pi, length.out = 36)
) %>%
  mutate(
    x = cos(angle) * speed,
    y = sin(angle) * speed
  )

# Create hour labels data for day and night
hour_labels <- data.frame(
  hour = c(6, 9, 12, 15, 18),  # Day hours
  angle = seq(pi, -pi, length.out = 5) + pi/2
) %>%
  mutate(
    x = cos(angle) * 70,
    y = sin(angle) * 70,
    label = sprintf("%02d:00", hour),
    time_of_day = "day"
) %>%
  bind_rows(
    data.frame(
      hour = c(18, 21, 0, 3, 6),  # Night hours
      angle = seq(pi, -pi, length.out = 5) + pi/2
    ) %>%
      mutate(
        x = cos(angle) * 70,
        y = sin(angle) * 70,
        label = sprintf("%02d:00", hour),
        time_of_day = "night"
      )
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(a64_avg, aes(x, y)) +
  # Density 2d
  geom_density_2d_filled() +
  # Speed limits
  geomtextpath::geom_textpath(data = ref_circles, aes(x = x, y = y, group = speed, label = paste0(speed, " mph")), color = "grey60", textcolor = "white", linetype = "dashed", hjust = 0.4) +
  # Hours
  geom_text(data = hour_labels, aes(x = x, y = y, label = label, color = if_else(time_of_day == "day", "#00E5B7", "#FF6B6B")), size = 3, vjust = rep(c(-0.2, 0.5, 1, 0.5, 1.2), 8), hjust = rep(c(0.8, 0.6, 0.5, 0.4, 0.2), 8), family = f1b) +
  # Arrow
  geom_curve(data = hour_labels %>% filter(hour == 18), aes(x = x - 16, y = y + 4, xend = x - 35, yend = y + 11, color = if_else(time_of_day == "day", "#00E5B7", "#FF6B6B")), curvature = -0.1, arrow = arrow(length = unit(0.5, "lines")), linewidth = 0.2) +
  scale_fill_viridis_d(option = "turbo", end = 0.9) +
  scale_color_identity() +
  scale_x_continuous(limits = c(-85, 85)) +
  scale_y_continuous(limits = c(-80, 80)) +
  coord_fixed(expand = FALSE) +
  facet_grid(vars(time_of_day), vars(str_wrap(name, 45)), switch = "y") +
  labs(
    title = "Fast after dark: speeding on the A64",
    subtitle = "Average traffic speeds on the A64 road in May 2021. Angle represents time of day and distance from center shows speed in mph, with color showing how\ncommon each speed is in 15-minute intervals. The data is split between day (6:00-18:00) and night (18:00-6:00) periods, with reference circles marking\n30 mph and 60 mph speed limits.",
    caption = "Data: National Highways · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(vjust = 0, margin = margin(0, 0, 10, 0)),
    strip.text.y = element_text(margin = margin(0, 5, 0, 0), angle = 90, size = 12),
    plot.title = element_text(family = f2, size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(6, 0, 10, 0), lineheight = 1),
    plot.margin = margin(10, 10, 10, 10)
  ) 
  
