library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8.8, units = "in", dpi = 320)

ocean_temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv')
# ocean_temperature_deployments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature_deployments.csv')

f1 <- "Karst"
pal <- rcartocolor::carto_pal(name = "Geyser")

ocean_mean_temp <- ocean_temperature |> 
  mutate(
    year = year(date),
    month = month(date, label = TRUE)
    ) |> 
  group_by(year, month, sensor_depth_at_low_tide_m) |> 
  summarise(mean_temperature_degree_c = mean(mean_temperature_degree_c, na.rm = TRUE)) |> 
  ungroup()
  
# Legend
l <- ggplot() +
  geom_tile(data = NULL, aes(x = 0, y = unique(ocean_mean_temp$sensor_depth_at_low_tide_m)), fill = "#6F7B89") +
  scale_y_reverse() +
  coord_polar() +
  labs(y = "depth\nincreases\ninward🞂") +
  theme_void(base_family = f1) +
  theme(
    axis.title.y = element_text(color = "#6F7B89", face = "bold", size = 8)
  )

# Plot
p <- ggplot(ocean_mean_temp, aes(x = 0, y = sensor_depth_at_low_tide_m, fill = mean_temperature_degree_c)) +
  geom_tile() +
  scale_fill_stepsn(colors = pal) +
  scale_y_reverse() +
  coord_polar() +
  facet_grid(vars(year), vars(month), scales = "free") +
  labs(
    title = "Nova Scotia's Ocean Pulse",
    subtitle = str_wrap("Seven years of daily coastal temperature measurements at depths of 2, 5, 10, 15, 20, 30, and 40 metres, from the Centre for Marine Applied Research. Each panel represents one month, with concentric rings indicating depth and color showing mean temperature.", 90),
    fill = "Mean temperature in degree Celsius",
    caption = "Source: Centre for Marine Applied Research · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(1.5, "lines"),
    legend.margin = margin(t= 10, b = 15),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(margin = margin(b = 1)),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(t = 5, b = 15), size = 12, hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = 10))
  )

p + 
  inset_element(l, t = 1.16, b = 1.06, l = 0.15, r = 0.33)
  
