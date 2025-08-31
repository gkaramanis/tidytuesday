library(tidyverse)
library(ggside)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv')

summer_hits <- billboard |> 
  select(song, artist, date, weeks_at_number_one) |> 
  mutate(
    week = week(date),
    month = month(date),
    year = year(date),
    decade = floor(year / 10) * 10
    ) |> 
  filter(month %in% 6:8) |> 
  add_count(decade) |> 
  filter(year >= 1960) |> 
  group_by(decade) |> 
  mutate(
    avg_weeks_decade = round(mean(weeks_at_number_one), 1),
    med_weeks_decade = round(median(weeks_at_number_one), 1)
    ) |> 
  ungroup()

pal <- MetBrewer::met.brewer("Tam")

f1 <- "Familjen Grotesk"

# https://time.com/7287405/song-of-the-summer-is-dead-essay/

t <- ggplot(data = summer_hits %>% distinct(decade, n, avg_weeks_decade, med_weeks_decade)) +
  geom_col(aes(x = decade + 4.5, y = n, fill = n), linewidth = 0.25, width = 9.5, color = NA) +
  marquee::geom_marquee(aes(x = decade + 4.5, y = n - 1.5, label = paste0("**", n, " songs**  \navg ", avg_weeks_decade, " w"), color = n > 50), family = f1, size = 3, lineheight = 0.9, vjust = 1) +
  scale_x_continuous(breaks = seq(1965, 2025, by = 10), limits = c(1959, 2030), labels = str_sub(paste0(seq(1960, 2020, by = 10), "s"), 3)) +
  # Add y labels to align with bottom chart
  scale_y_continuous(breaks = 10, label = "Aug", sec.axis = dup_axis(breaks = 10, label = "W20")) +
  scale_color_manual(values = c("black", "white")) +
  MetBrewer::scale_fill_met_c("VanGogh3") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Summer number ones are less frequent but spend more weeks on the top",
    subtitle = str_wrap("Songs that reached number one on the Billboard Hot 100 for the first time during June, July, or August from 1960 to 2024. Since the 1990s, fewer songs have debuted at number one in summer, but those that do spend more total weeks at number one, whether consecutive or not.", 120)
  ) +
  theme_minimal(base_family = f1, base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(margin = margin(b = 20))
  )

b <- ggplot() +
  # geom_segment(data = NULL, aes(x = seq(1969.5, 2019.5, by = 10), y = 21, xend = seq(1969.5, 2019.5, by = 10), yend = 36), color = "#cbcfd2", linewidth = 0.25) +
  geom_tile(data = summer_hits, aes(x = year, y = week, fill = weeks_at_number_one), width = 0.9, height = 0.9) +
  ggrepel::geom_text_repel(data = summer_hits |> group_by(decade) |> slice_max(order_by = weeks_at_number_one, n = 1) |> ungroup(), aes(x = year, y = week, label = paste0(song, "\n", weeks_at_number_one, " weeks"), segment.color = weeks_at_number_one), size = 2.5, family = f1, color = "black", max.overlaps = Inf, segment.size = 0.25, direction = "y", vjust = -6, nudge_y = 4, bg.color = "grey98", lineheight = 0.95) +
  geom_point(data = summer_hits |> group_by(decade) |> slice_max(order_by = weeks_at_number_one, n = 1) |> ungroup(), aes(x = year, y = week), color = "white", size = 0.1) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10), limits = c(1959, 2030), minor_breaks = seq(1960, 2024, by = 1), guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_reverse(limits = c(36, 18), breaks = c(24, 28, 33), labels = c("Jun", "Jul", "Aug"), sec.axis = dup_axis(breaks = seq(22, 35, 2), labels = paste0("W", seq(22, 35, 2)))) +
  scale_fill_stepsn(colors = pal, breaks = c(4, 8, 12, 16)) +
  scale_color_stepsn(colors = pal, breaks = c(4, 8, 12, 16), limits = c(1, 19), aesthetics = c("segment.color"), guide = FALSE) +
  coord_fixed(expand = FALSE) +
  labs(
    caption = "Source: Billboard Hot 100 Number Ones Database · Graphic: Georgios Karamanis",
    fill = "Weeks at #1"
  ) +
  guides(fill = guide_colorsteps(show.limits = TRUE)) +
  theme_minimal(base_family = f1, base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 10, color = "#767b8d"),
    legend.text = element_text(color = "#767b8d"),
    legend.key.width = unit(1.8, "lines"),
    legend.key.height = unit(0.5, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y.left = element_text(size = 8), 
    axis.text.y.right = element_text(size = 6), 
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "#a7adb2", linewidth = 0.25),
    axis.ticks.length = unit(0.4, "lines"),
    axis.minor.ticks.length = rel(0.5),
    axis.ticks.y = element_blank()
  )


t + b +
  plot_layout(ncol = 1, heights = c(1.5, 2)) +
  plot_annotation(
    theme = theme_minimal() +
      theme(
        plot.background = element_rect(fill = "gray99", color = NA)
      )
  )

