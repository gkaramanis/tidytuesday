library(tidyverse)
library(broom)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

options(scipen=999)

england_wales_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/england_wales_names.csv')
ni_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/ni_names.csv')
scotland_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/scotland_names.csv')

names_ratio <- bind_rows(england_wales_names, ni_names, scotland_names) |>
  filter(between(Year, 1997, 2024)) |> 
  filter(!is.na(Name), !is.na(Number)) |> 
  group_by(Name, Year, Sex) |> 
  summarise(
    Number = sum(Number)
  ) |> 
  ungroup() |> 
  pivot_wider(id_cols = c(Year, Name), values_from = Number, names_from = Sex) |>
  # keep single-sex years: missing sex is suppressed (<3), treat as 0 + pseudocount
  mutate(
    Boy = coalesce(Boy, 0),
    Girl = coalesce(Girl, 0),
    ratio = (Girl + 0.5) / (Boy + 0.5)
  )

trends <- names_ratio |>
    group_by(Name) |>
    filter(
      n_distinct(Year) >= 5,
      any(Girl >= 20 & Boy < Girl),   # genuinely a girls' name at some point
      any(Boy  >= 20 & Girl < Boy)    # and a boys' name at some point
    ) |>
    summarise(
      fit = list(lm(log(ratio) ~ Year)),
      y0 = min(Year), y1 = max(Year),
      observed_crossed = any(log(ratio) > 0) & any(log(ratio) < 0),
      .groups = "drop"
    ) |>
    mutate(
      slope        = map_dbl(fit, ~ coef(.x)[2]),
      r2           = map_dbl(fit, ~ summary(.x)$r.squared),
      fit_start    = map2_dbl(fit, y0, ~ predict(.x, tibble(Year = .y))),
      fit_end      = map2_dbl(fit, y1, ~ predict(.x, tibble(Year = .y))),
      total_change = fit_end - fit_start
    )

top_trends <- trends |>
  filter(observed_crossed, r2 > 0.5) |>
  slice_max(abs(total_change), n = 12)


bg <- names_ratio |> 
  rename(name_bg = Name)

fg <- names_ratio |> 
  semi_join(top_trends, by = c("Name")) |> 
  left_join(top_trends) |> 
  mutate(Name = fct_reorder(Name, slope))

fg_labels_max <- fg |> 
  group_by(Name) |> 
  filter(Year == max(Year)) |> 
  ungroup()

fg_labels_min <- fg |> 
  group_by(Name) |> 
  filter(Year == min(Year)) |> 
  ungroup()

f1 <- "Karst"
f2 <- "Bricolage Grotesque 12pt Condensed"

ggplot() +
  # geom_path(data = bg, aes(ratio, Year, group = interaction(name_bg)), linewidth = 0.05, alpha = 0.1) +
  geom_vline(xintercept = 1, color = "white", linewidth = 0.2) +
  geom_path(data = fg, aes(ratio, Year, group = Name, color = slope > 0), linewidth = 1) +
  shadowtext::geom_shadowtext(data = fg_labels_max, aes(ratio, Year, label = Boy), nudge_x = -0.2, hjust = 1, size = 3.5, vjust = 0, family = f2, color = "grey20", bg.color = "white") +
  shadowtext::geom_shadowtext(data = fg_labels_max, aes(ratio, Year, label = Girl), nudge_x = 0.2, hjust = 0, size = 3.5, vjust = 0, family = f2, color = "grey20", bg.color = "white") +
  shadowtext::geom_shadowtext(data = fg_labels_min, aes(ratio, Year, label = Boy), nudge_x = -0.2, hjust = 1, size = 3.5, vjust = 1, family = f2, color = "grey20", bg.color = "white") +
  shadowtext::geom_shadowtext(data = fg_labels_min, aes(ratio, Year, label = Girl), nudge_x = 0.2, hjust = 0, size = 3.5, vjust = 1, family = f2, color = "grey20", bg.color = "white") +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("100× boys", "10×", "same", "10×", "100× girls"),
    limits = c(0.01, 100),
    oob = scales::oob_squish
  ) +
  scale_y_reverse(breaks = seq(2005, 2025, 10)) +
  scale_color_manual(values = c("#f58231", "#3cb44b")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  facet_wrap(vars(Name)) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks.x = element_line(),
    axis.text = element_text(family = f2, size = 10),
    strip.text = element_text(face = "bold", size = 12)
  )

record_polaroid()
