library(tidyverse)
library(camcorder)
library(marquee)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

options(scipen = 999)

england_wales_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/england_wales_names.csv')
ni_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/ni_names.csv')
scotland_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/scotland_names.csv')

# minimum boys + girls per year to keep a name-year (counts below ~3 are
# suppressed in the source data, so % swings wildly at low totals)
min_count <- 20

shares <- bind_rows(england_wales_names, ni_names, scotland_names) |>
  filter(between(Year, 1997, 2024), !is.na(Name), !is.na(Number)) |>
  group_by(Name, Year, Sex) |>
  summarise(Number = sum(Number), .groups = "drop") |>
  pivot_wider(id_cols = c(Year, Name), values_from = Number, names_from = Sex) |>
  mutate(
    Boy = coalesce(Boy, 0),
    Girl = coalesce(Girl, 0),
    total = Boy + Girl,
    girl_share = Girl / total
  ) |>
  # keep only names that clear the volume floor in every year they appear
  group_by(Name) |>
  filter(all(total >= min_count)) |>
  ungroup()

# select names that genuinely crossed 50%, with real volume on both sides
trends <- shares |>
  group_by(Name) |>
  filter(
    n_distinct(Year) >= 5,
    any(girl_share > 0.5),   # girl-majority at some point
    any(girl_share < 0.5)    # boy-majority at some point
  ) |>
  summarise(
    y0 = min(Year), y1 = max(Year),
    slope = coef(lm(girl_share ~ Year))[2],
    start_share = girl_share[which.min(Year)],
    end_share = girl_share[which.max(Year)],
    share_range = max(girl_share) - min(girl_share),  # total excursion, keeps wobblers/U-shapes
    .groups = "drop"
  )

top_trends <- trends |>
  slice_max(share_range, n = 12)

names <- shares |>
  semi_join(top_trends, by = "Name") |>
  left_join(top_trends, by = "Name") |>
  mutate(Name = fct_reorder(Name, slope)) |>
  select(Name, Year, Boy, Girl) |>
  pivot_longer(c(Boy, Girl), names_to = "Sex", values_to = "Number")

# counts at the first and last year, placed around the 50/50 line
names_labels <- shares |>
  semi_join(top_trends, by = "Name") |>
  left_join(top_trends, by = "Name") |>
  group_by(Name) |>
  filter(Year == min(Year) | Year == max(Year)) |>
  ungroup() |>
  mutate(Name = fct_reorder(Name, slope))

names_top <- filter(names_labels, Year == min(Year), .by = Name)
names_bottom <- filter(names_labels, Year == max(Year), .by = Name)

f1 <- "Karst"
f2 <- "Bricolage Grotesque 12pt Condensed"

col_boy <- "#009E73"
col_girl <- "#E69F00"

subtitle_style <- classic_style(body_font = f2) |>
  modify_style("boy", color = col_boy, weight = "bold") |>
  modify_style("girl", color = col_girl, weight = "bold")

ggplot(names, aes(x = Number, y = Year, fill = Sex)) +
  geom_col(position = "fill", orientation = "y", width = 1) +
  geom_vline(xintercept = 0.5, color = "grey99", linewidth = 0.3) +
  geom_vline(xintercept = 0.25, color = "grey99", linewidth = 0.3, linetype = "dotted") +
  geom_vline(xintercept = 0.75, color = "grey99", linewidth = 0.3, linetype = "dotted") +
  shadowtext::geom_shadowtext(data = names_top, aes(x = girl_share, y = Year, label = Boy), inherit.aes = FALSE, nudge_x = 0.02, hjust = 0, size = 3, family = f2, color = "white", bg.color = "black") +
  shadowtext::geom_shadowtext(data = names_top, aes(x = girl_share, y = Year, label = Girl), inherit.aes = FALSE, nudge_x = -0.02, hjust = 1, size = 3, family = f2, color = "white", bg.color = "black") +
  shadowtext::geom_shadowtext(data = names_bottom, aes(x = girl_share, y = Year, label = Boy), inherit.aes = FALSE, nudge_x = 0.02, hjust = 0, size = 3, family = f2, color = "white", bg.color = "black") +
  shadowtext::geom_shadowtext(data = names_bottom, aes(x = girl_share, y = Year, label = Girl), inherit.aes = FALSE, nudge_x = -0.02, hjust = 1, size = 3, family = f2, color = "white", bg.color = "black") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("girls →", "", "even", "", "← boys")) +
  scale_y_reverse(breaks = seq(2000, 2020, 10)) +
  scale_fill_manual(values = c(Boy = col_boy, Girl = col_girl)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  facet_wrap(vars(Name)) +
  labs(
    title = "Shifting names",
    subtitle = "Some first names are given to both boys and girls. These twelve UK names saw the balance between the two shift the most from 1997 to 2024. Each panel is one name and every bar within it is a single year. The colors show how that year's babies divided between {.girl girls} and {.boy boys}, and the center line marks an even split. The numbers are how many of each were born in the first and last year.",
    caption = "Source: ONS, NRS & NISRA · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    axis.ticks.x = element_line(),
    panel.spacing = unit(1, "lines"),
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 9),
    axis.text.x = element_text(hjust = c(0, 0.5, 0.5, 0.5, 1), color = c(col_girl, NA, "grey20", NA, col_boy), face = c("bold", rep("plain", 3), "bold")),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_marquee(style = subtitle_style, width = 0.99, size = 11, lineheight = 1.1, margin = margin(b = 12)),
    plot.caption = element_text(family = f2, hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
