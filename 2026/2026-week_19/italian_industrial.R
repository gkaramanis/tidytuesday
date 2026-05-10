library(tidyverse)
library(rvest)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# food_beverages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv')
# textiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/textiles.csv')
transport <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/transport.csv') |> 
  janitor::clean_names()

wiki_cars_raw <- read_html("https://en.wikipedia.org/wiki/Automotive_industry_in_Italy") |> 
  html_table() %>%
  .[[2]] 

wiki_cars <- wiki_cars_raw |> 
  janitor::clean_names() |> 
  mutate(
    units = parse_number(units),
    grp = cumsum(c(1, diff(year) > 1))
    )

annot_df <- tribble(
  ~year, ~label,
  1939, "Ferrari founded",
  1940, "Italy enters WWII",
  1945, "End of WWII",
  1957, "Fiat 500 launched",
  1963, "Lamborghini founded",
  1973, "Oil crisis",
  1978, "Lamborghini bankrupt",
  1989, "Peak production\n2.2M units",
  1993, "Fiat Punto launched",
  1998, "Lamborghini to VW",
  2001, "Fiat: 90% of production",
  2008, "Financial crisis",
  2014, "Fiat Chrysler formed",
  2021, "Stellantis formed"
)

it_cars <- transport |>
  full_join(wiki_cars) |> 
  left_join(annot_df) |> 
  select(year, passenger_cars, units, grp, label) |> 
  filter(!is.na(passenger_cars) | !is.na(units)) |> 
  mutate(label_y = if_else(!is.na(label), coalesce(passenger_cars, units), NA_real_))

f1 <- "Karst"
f2 <- "Signika"

col1 <- "#2D3142"
col2 <- "#C9734A"

ggplot(it_cars, aes(x = year)) +
  geom_area(aes(y = passenger_cars), linewidth = 1.5, color = col1, alpha = 0.08) +
  geom_point(aes(y = passenger_cars), size = 1, color = col1) +
  geom_line(aes(y = units, group = grp), color = col2, linewidth = 1.5) +
  geom_point(aes(y = units), color = col2, size = 1) +
  ggrepel::geom_label_repel(aes(y = label_y, label = label), nudge_y = 1.2e6, direction = "y", segment.size = 0.2, label.padding = 0.45, point.padding = 1, label.size = 0.05, label.r = 0.6, fill = alpha("white", 0.85), family = f2, fontface = "bold") +
  scale_x_continuous(minor_breaks = 1910:2024, breaks = seq(1910, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 2.5e6, 0.5e6), labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  guides(
    x = guide_axis(minor.ticks = TRUE)
  ) +
  labs(
    title = "2.2 Million Cars",
    subtitle = "Italy's peak production year was 1989, when manufacturers turned out 2.2 million passenger cars. {.#2D3142 **Istat**} industrial production statistics cover the period from 1919; {.#C9734A **Wikipedia's**} production figures extend the series to 2024, together tracing more than a century of Italian car manufacturing across wars, oil shocks, and corporate upheaval.",
    caption = "Source: Istat, Automotive industry in Italy (Wikipedia) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#FAF8F4", color = NA),
    axis.title = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "grey80", linewidth = 1),
    axis.ticks.length.x = unit(0.3, "lines"),
    axis.minor.ticks.x.bottom = element_line(color = "grey80", linewidth = 0.5),
    axis.text = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(size = 24, face = "bold", family = f2, color = col1),
    plot.subtitle = marquee::element_marquee(size = 12, lineheight = 1, width = 1, margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "grey50", hjust = 0, family = f2, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
