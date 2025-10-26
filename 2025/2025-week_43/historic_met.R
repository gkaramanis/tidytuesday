library(tidyverse)
library(flexfont)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10.2, height = 8, units = "in", dpi = 320)

historic_station_met <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv')

station_meta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/station_meta.csv')

rain <- historic_station_met |> 
  left_join(station_meta, by = "station") |> 
  group_by(year, station) |> 
  reframe(station_name, lng, lat, rain = median(rain, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct() |> 
  filter(year >= 2012) |> 
  add_count(station) |> 
  filter(n == max(n)) |> 
  group_by(station) |> 
  mutate(
    name_length = str_length(station),
    year_start = 2012,
    year_end = 2024,
    total_years = year_end - year_start + 1,
    # Repeat the station name enough times to cover all years
    repeats_needed = ceiling(total_years / name_length),
    repeated_name = str_c(rep(unique(station), unique(repeats_needed)), collapse = ""),
    # Take the last 'total_years' characters to work backwards from 2024
    full_name_for_years = str_sub(repeated_name, -total_years, -1),
    flex_name = list(flex_coords(full_name_for_years, w = 1.5, vgap = 0.3, hgap = 0.2)),
    max_rain = max(rain, na.rm = TRUE),
    # Mark the last complete name: characters from position (total_years - name_length + 1) to total_years
    last_name_start = total_years - name_length + 1
  ) |> 
  ungroup() |> 
  filter(max_rain > 100)

rain_unlisted <- rain |> 
  filter(year >= year_start) |>
  unnest(flex_name) |> 
  mutate(
    letter_year = year_start + chr_idx - 1,
    # Mark if this character is part of the last complete repeat of the name
    is_last_repeat = chr_idx >= last_name_start
  ) |> 
  filter(year == letter_year) |> 
  group_by(station, year, chr_idx) |> 
  mutate(
    x2 = scales::rescale(x, to = c(-0.35, 0.35)) + unique(year),
    y2 = scales::rescale(y, to = c(10, unique(rain) - 4)) # -4 to account for linewidth = 1 of geom_path
  ) |> 
  ungroup() |> 
  mutate(station_name = fct_reorder(station_name, -rain))

f1 <- "Outfit"
f2 <- "LINE Seed Sans"
f3 <- "Inclusive Sans"

ggplot(rain_unlisted) +
  # geom_col(aes(x = year, y = rain), stat = "unique", fill = "#5a93c7") +
  geom_segment(aes(x = -Inf, xend = Inf, y = max_rain, yend = max_rain), color = "#c7375f", linewidth = 0.2, stat = "unique") +
  geom_tile(data = data.frame(x = 2011:2024, y = 0), aes(x, y), width = 0.9, height = 5, fill = "#5a93c7", color = NA) +
  geom_path(aes(x2, y2, group = interaction(chr_idx, stroke_idx), color = is_last_repeat), na.rm = TRUE, linewidth = 1, lineend = "round") +
  scale_x_continuous(limits = c(2011.5, 2024.5), breaks = seq(2010, 2024, by = 2), expand = 0) +
  scale_y_continuous(breaks = seq(0, 200, 100), labels = c("", 100, 200), minor_breaks = seq(0, 225, 20)) +
  scale_color_manual(values = c("FALSE" = "#d4e4f0", "TRUE" = "#236494")) +
  facet_wrap(vars(station_name), ncol = 2, strip.position = "bottom", dir = "h") +
  coord_cartesian(clip = "off") +
  labs(
    title = str_to_upper("Reading the rain"),
    subtitle = str_wrap("Median annual rainfall (mm) at UK weather stations from 2012 to 2024. Stations shown have complete 13-year data and maximum rainfall > 100 mm. Letter height represents yearly rainfall. Red line shows maximum rainfall for each station. Eskdalemuir recorded the highest rainfall (213.3 mm in 2020), while 2022 was notably dry across most stations.", 120),
    caption = "Data: Met Office | Chart: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fbf9f9", color = NA),
    strip.text = element_text(hjust = 1, size = 8, margin = margin(t = 0, b = 3), color = "#11487a"),
    axis.title = element_blank(),
    axis.text = element_text(size = 6, color = "grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_text(size = 22, hjust = 0, color = "#c7375f", family = f2, face = "bold", margin = margin(b = 3)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0, b = 20), color = "grey20", family = f3),
    plot.caption = element_text(size = 7, color = "grey40", hjust = 0, margin = margin(t = 10))
  )

record_polaroid()
