library(tidyverse)
library(treemapify)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

health <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-29/health.csv')


too_many_even <- health |> 
  select(HL_FCL_HOS_2024, HL_FCL_PHA_2024) |> 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(even_odd = if_else(value %% 2 == 0, "Even", "Odd")) |> 
  count(metric, value, even_odd, sort = TRUE) |> 
  group_by(metric) |> 
  mutate(i = row_number()) |>
  filter(i <= 25) |>
  ungroup() 
  
f1 <- "Work Sans"
f2 <- "Input Mono Narrow"

ggplot(too_many_even, aes(x = i, y = n, fill = even_odd)) +
  geom_col() +
  geom_text(aes(label = value, y = if_else(even_odd == "Even", n + 75, n + 180)), family = f2) +
  scale_fill_manual(values = c("Even" = "lightblue", "Odd" = "lightcoral")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  facet_wrap(vars(metric), ncol = 1) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )


hp_counts <- health |>
  filter(!is.na(HL_FCL_HOS_2024)) |>
  filter(HL_FCL_HOS_2024 <= quantile(HL_FCL_HOS_2024, 0.9)) |>
  count(value = HL_FCL_HOS_2024)

# First plot
p1 <- ggplot(hp_counts, aes(x = value, y = n)) +
  geom_col(fill = "#1d4e89") +
  scale_x_continuous(breaks = c(1:10, seq(12, max(hp_counts$value), by = 2)), limits = c(0.5, max(hp_counts$value) + 0.5)) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(0, 2300)) +
  annotate(
    "text", x = 0.6, y = 2150, hjust = 0,, lineheight = 0.95, size = 5, family = f1,
    label = "No urban centre has a single hospital"
  ) +
  annotate(
    "segment", x = 1, xend = 1, y = 2060, yend = 1750,
    arrow = arrow(length = unit(2, "mm")), linewidth = 0.3
  ) +
  annotate(
    "text", x = 9.5, y = 1200, hjust = 0, lineheight = 0.95, size = 6, family = f1,
    label = "95% of hospital counts are even"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(family = f2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Second plot
region_pos <- health |>
  summarise(
    total = n(),
    share_missing = mean(is.na(HL_FCL_HOS_2024)),
    .by = GC_DEV_USR_2025
  ) |>
  rename(region = GC_DEV_USR_2025) |>
  arrange(share_missing) |>
  mutate(ymax = cumsum(total), ymin = ymax - total, ymid = (ymin + ymax) / 2) |> 
  mutate(region = if_else(region == "Oceania", "← Pacific Islands", region))

p2 <- health |>
  count(
    region = GC_DEV_USR_2025,
    hospital_count = if_else(is.na(HL_FCL_HOS_2024), "No count", "Count")
  ) |>
  left_join(region_pos, by = "region") |>
  arrange(region, hospital_count) |>
  mutate(xmax = cumsum(n) / total, xmin = xmax - n / total, .by = region) |>
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = hospital_count), colour = "grey99") +
  scale_x_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  scale_y_continuous(breaks = region_pos$ymid, labels = str_replace(region_pos$region, "and ", "&\n"), expand = c(0, 0), position = "right") +
  scale_fill_manual(values = c("Count" = "#1d4e89", "No count" = "gray90")) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 8),
    axis.text.y = element_text(hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 30)
  )

p1 + p2 +
  plot_layout(ncol = 2, widths = c(2, 1)) +
  plot_annotation(
    title = "Two hospitals, or none*",
    subtitle = " The EU's Joint Research Centre (JRC) counts hospitals in 11,422 urban centres worldwide, using OpenStreetMap. On the left, bar height is the number of urban centres with each number of hospitals (x axis). 95% of the counts are even and none is 1, which suggests hospitals are counted twice. On the right, each region's {.#1d4e89 **blue bar**} is its share of urban centres with a hospital count. Thicker bars mean more urban centres. 45% have none, from 5% in Europe to 70% in Central and Southern Asia. ***Check your data, even when it comes from a trusted source!**",
    caption = "Source: JRC GHS Urban Centre Database R2024A, built from OpenStreetMap · Graphic: Georgios Karamanis",
    theme = theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.title = element_text(family = f1, size = 24, face = "bold"),
      plot.subtitle = marquee::element_marquee(family = f1, size = 11, width = 1, lineheight = 1.1),
      plot.caption = element_text(family = f1, size = 9, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

