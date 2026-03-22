library(tidyverse)
library(rnaturalearth)
library(sf)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

monthly_mortality_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv')

# Midpoints: 1&2 = mean of (58.5,8.5) and (59.1,6.1); 12&13 = mean of (70.5,24.5) and (70.6,29.0)
area_coords <- tribble(
  ~region,    ~lat,    ~lon,    ~name,
  "1 & 2",    58.8,    7.3,     "Skagerrak–Ryfylke",
  "3",        60.1,    4.9,     "Karmøy–Sotra",
  "4",        60.8,    4.8,     "Nordhordland–Stadt",
  "5",        62.5,    6.0,     "Stadt–Hustadvika",
  "6",        63.4,    8.2,     "Nordmøre–S. Trøndelag",
  "7",        64.4,    11.5,    "N. Trøndelag–Bindal",
  "8",        66.3,    13.8,    "Helgeland–Bodø",
  "9",        68.0,    14.5,    "Vestfjorden–Vesterålen",
  "10",       69.1,    17.0,    "Andøya–Senja",
  "11",       70.2,    22.0,    "Kvænangen–Loppa",
  "12 & 13",  70.55,   26.75,   "Finnmark"
)

area_rate <- monthly_mortality_data |>
  filter(species == "salmon", geo_group == "area") |>
  mutate(period = ifelse(year(date) <= 2022, "early", "recent")) |>
  group_by(period, region) |>
  summarise(mean_median = mean(median, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = mean_median) |>
  mutate(
    delta = (recent - early) / early,
    mean_all = (early + recent) / 2
  ) |>
  left_join(area_coords, by = "region")

norway <- ne_countries(scale = "large", country = "Norway", returnclass = "sf")
neighbors <- ne_countries(scale = "large", returnclass = "sf") |>
  filter(name %in% c("Sweden", "Finland", "Russia", "Denmark"))

f1 <- "Karst"
f2 <- "Metropolis"

sea_col      <- "#9BB8D4"
norway_col   <- "#F7F5F0"
neighbor_col <- "#D4CFC4"

annot <- "### The coast is not fine\n\nThe Norwegian Veterinary Institute tracks salmon farming mortality across 13 coastal production areas, grouped into 11 here.\

At its worst, one in six fish died before harvest. Roughly 38% of deaths are attributed to infectious disease and another third to physical trauma from de-licing. The rest have no clear cause.\

Each circle is a production zone. Size reflects mean monthly mortality rate across 2020–2025. {.#A93226 **Red**} zones saw the rate rise between 2020–22 and 2023–25. {.#2166AC **Blue**} zones saw it fall.\

Nordmøre–S. Trøndelag and Stadt–Hustadvika deteriorated most. Kvænangen–Loppa and Finnmark improved most.\n\n
>Source: Norwegian Veterinary Institute\  
>Graphic: Georgios Karamanis"

annot_style <- classic_style() |>
  modify_style("body", border_radius = 10) |>
  modify_style("h3", family = f1, padding = trbl(left = 10, right = 10)) |>
  modify_style("qb", size = relative(0.75), lineheight = 1.1) |>
  modify_style("p", color = "black", padding = trbl(left = 10, right = 10))

ggplot() +
  geom_sf(data = neighbors, fill = neighbor_col, color = "#9A9488", linewidth = 0.1) +
  geom_sf(data = norway, fill = norway_col, color = "grey35", linewidth = 0.25) +
  geom_point(data = area_rate, aes(x = lon, y = lat, size = mean_all, fill = delta), alpha = 0.85, stroke = 0.5, shape = 21, color = "white") +
  shadowtext::geom_shadowtext(data = area_rate, aes(x = lon, y = lat, label = scales::percent(delta, 1)), color = "white", fontface = "bold", size = 3.5, family = f1) +
  ggrepel::geom_text_repel(data = area_rate, aes(x = lon, y = lat, label = name), family = f1, fontface = "bold", seed = 99, position = ggpp::position_nudge_line(direction = "split", x = 1, y = 0.65), segment.size = 0, size = 3, color = "grey5", bg.color = "white", point.padding = 15, force_pull = 0) +
  geom_marquee(data = NULL, aes(x = 20.5, y = 67.7, label = annot), family = f2, size = 3.5, width = 0.37, hjust = 0, vjust = 1, fill = alpha(norway_col, 0.8), color = "grey15", style = annot_style) +
  MetBrewer::scale_fill_met_c("Johnson", direction = 1) +
  scale_size_area(max_size = 16, labels = scales::label_percent()) +
  coord_sf(xlim = c(0, 35.25), ylim = c(57.5, 72.28), crs = sf::st_crs("+proj=laea +lat_0=65 +lon_0=16"), default_crs = 4326) +
  guides(
    size = guide_legend(title = "Mean monthly mortality rate", direction = "horizontal"),
    fill = "none"
    ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = c(0.2, 0.9),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, color = "white", family = f1, size = 9),
    legend.text.position = "bottom",
    legend.text = element_text(color = "white", family = f1, size = 8),
    plot.background = element_rect(fill = sea_col, color = NA),
    panel.background = element_rect(fill = sea_col, color = NA)
  )

record_polaroid()
  