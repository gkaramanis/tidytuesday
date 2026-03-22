library(tidyverse)
library(rnaturalearth)
library(sf)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

monthly_losses_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv')

# monthly_mortality_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv')

area_coords <- tribble(
  ~region, ~lat,   ~lon,   ~name,
  "1",     58.5,   8.5,    "Skagerrak/Oslofjord",
  "2",     59.1,   6.1,    "Ryfylke",
  "3",     60.1,   4.9,    "Karmøy–Sotra",
  "4",     60.8,   4.8,    "Nordhordland–Stadt",
  "5",     62.5,   6.0,    "Stadt–Hustadvika",
  "6",     63.4,   8.2,    "Nordmøre–S. Trøndelag",
  "7",     64.4,   11.5,   "N. Trøndelag–Bindal",
  "8",     66.3,   13.8,   "Helgeland–Bodø",
  "9",     68.0,   14.5,   "Vestfjorden–Vesterålen",
  "10",    69.1,   17.0,   "Andøya–Senja",
  "11",    70.2,   22.0,   "Kvænangen–Loppa",
  "12",    70.5,   24.5,   "Vest-Finnmark",
  "13",    70.6,   29.0,   "Øst-Finnmark"
)

area_delta <- monthly_losses_data |>
  filter(species == "salmon", geo_group == "area") |>
  mutate(period = ifelse(year(date) <= 2022, "early", "recent")) |>
  group_by(period, region) |>
  summarise(total_losses = sum(losses), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = total_losses) |>
  mutate(delta = (recent - early) / early) |>
  left_join(area_coords, by = "region")

norway <- ne_countries(scale = "large", country = "Norway", returnclass = "sf")
neighbors <- ne_countries(scale = "large", returnclass = "sf") |>
  filter(name %in% c("Sweden", "Finland", "Russia", "Denmark"))

f1 <- "Karst"
f2 <- "Metropolis"

sea_col      <- "#C8DCF0"
norway_col   <- "#F7F5F0"
neighbor_col <- "#E3DFD5"

annot <- "### Death along the coast\n\nIn 2023, one in six farmed salmon in Norway died before reaching harvest, the worst rate on record. Infectious disease accounts for roughly 38% of losses; physical trauma from de-licing operations another 33%. The rest are largely unexplained.\

The Norwegian Veterinary Institute tracks losses across 13 production areas.\

Circles show the change in number of deaths between 2020–22 and 2023–25 — not mortality rate. Circle size shows the magnitude of change; {.#A93226 **red**} means more deaths, {.#2166AC **blue**} fewer.\

Note: the baseline includes pandemic-affected years, so some changes may be inflated. Øst-Finnmark (13), the largest blue circle, recorded the steepest drop but is also the smallest zone by far.\n\n
>Source: Norwegian Veterinary Institute  
>Graphic: Georgios Karamanis"

annot_style <- classic_style() |>                                             
  modify_style("body", border_radius = 10) |>               
  modify_style("h3", family = f1, padding = trbl(left = 10, right = 10)) |>   
  modify_style("qb", size = relative(0.75), lineheight = 1.1) |>                          
  modify_style("p", padding = trbl(left = 10, right = 10))

ggplot() +
  geom_sf(data = neighbors, fill = neighbor_col, color = "grey70", linewidth = 0.1) +
  geom_sf(data = norway, fill = norway_col, color = "grey35", linewidth = 0.25) +
  geom_point(data = area_delta, aes(x = lon, y = lat, size = abs(delta), fill = delta), alpha = 0.85, stroke = 0.5, shape = 21, color = "white") +
  shadowtext::geom_shadowtext(data = area_delta, aes(x = lon, y = lat, label = scales::percent(delta, 2)), color = "white", fontface = "bold", size = 3.5, family = f1) +
  ggrepel::geom_text_repel(data = area_delta, aes(x = lon, y = lat, label = name), family = f1, fontface = "bold", seed = 99, position = ggpp::position_nudge_line(direction = "split", x = 0.8, y = 0.65), segment.size = 0, size = 3, color = "grey5", bg.color = "white") +
  geom_marquee(data = NULL, aes(x = 18.5, y = 66, label = annot), family = f2, size = 3.5, width = 0.37, hjust = 0, vjust = 1, fill = alpha("white", 0.5), color = "grey15", style = annot_style) +
  MetBrewer::scale_fill_met_c("Benedictus", direction = -1) +
  scale_size_area(max_size = 20, guide = "none") +
  coord_sf(xlim = c(-1, 34.25), ylim = c(57, 72),  crs = sf::st_crs("+proj=laea +lat_0=65 +lon_0=16"), default_crs = 4326) +
  labs(fill = NULL) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = sea_col, color = NA),
    panel.background = element_rect(fill = sea_col, color = NA)
    )

record_polaroid()
