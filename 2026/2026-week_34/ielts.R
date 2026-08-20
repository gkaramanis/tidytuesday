library(tidyverse)
library(ggbeeswarm)
library(sf)
library(camcorder)
library(marquee)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

# demo_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_first_language.csv')
# demo_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_nationality.csv')
# demo_by_reasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_reasons.csv')
# performance_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_first_language.csv')
performance_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_nationality.csv')

perf_nat <- performance_by_nationality |> 
  filter(type == "Academic", year == max(year), part == "overall") |> 
  mutate(score = round(score, 2)) 

p <- ggplot(perf_nat, aes(x = score, y = 0)) +
  geom_beeswarm(cex = 12) 

countries_xy <- ggplot_build(p)$data[[1]] |>
  as_tibble() |>
  select(x, y) |>
  mutate(nationality = performance_by_nationality |> 
  filter(type == "Academic", year == max(year), part == "overall") |> 
  pull(nationality)) |> 
  mutate(code = countrycode::countrycode(nationality, origin = "country.name", destination = "iso3c")) 

keep_mainland <- function(g) {
  parts <- st_cast(st_sfc(g), "POLYGON")
  parts[which.max(st_area(parts))][[1]]
}

country_polygons <- rnaturalearth::countries110 |> 
  select(code = ISO_A3_EH, name = NAME) |> 
  right_join(countries_xy, by = "code") |> 
  mutate(
    geometry = case_when(
    code == "HKG" ~ read_sf("https://geo2day.com/asia/china/hong_kong.geojson") |> st_geometry(),
    TRUE ~ geometry
  ),
    geometry = st_sfc(map2(
    geometry, code,
    ~ if (.y %in% c("FRA", "RUS")) keep_mainland(.x) else .x
  )),
  name = if_else(is.na(name), nationality, name)
)

place_glyph <- function(g, x, y, size = 0.12) {
  g  <- st_sfc(g)
  bb <- st_bbox(g)
  ctr  <- c((bb["xmin"] + bb["xmax"]) / 2, (bb["ymin"] + bb["ymax"]) / 2)
  span <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])   # normalise per country
  (g - ctr) / span * size + c(x, y)
}

placed <- country_polygons |>
  filter(!st_is_empty(geometry)) |>
  mutate(geometry = do.call(c, purrr::pmap(list(geometry, x, y), place_glyph))) |>
  st_set_crs(NA) |>
  st_as_sf()

# Territories where English is official or primary language
# source: https://en.wikipedia.org/wiki/List_of_countries_and_territories_where_English_is_an_official_language
countries_eng <- c("Nigeria", "Ghana", "Kenya", "India", "Pakistan", "Philippines", "Hong Kong", "Sri Lanka", "Malaysia")

labels_df <- placed |>
  mutate(
    y_mid = map_dbl(geometry, ~ st_bbox(.x)[["ymin"]] + (st_bbox(.x)[["ymax"]] - st_bbox(.x)[["ymin"]]) / 2),
    x_right = map_dbl(geometry, ~ st_bbox(.x)[["xmax"]]),
  ) |>
  st_drop_geometry()

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Graphik"

ggplot(placed, aes(x = x, y = y)) +
  geom_vline(xintercept = seq(5.4, 7.6, 0.1), linewidth = 0.05, color = "#D98A29") +
  geom_vline(xintercept = seq(5.5, 7.5, 0.5), linewidth = 0.1, color = "#D98A29") +
  geom_sf(aes(fill = name %in% countries_eng, color = after_scale(colorspace::darken(fill, 0.7)))) +
  shadowtext::geom_shadowtext(data = labels_df, aes(x = x_right, y = y_mid, label = paste(name, "\n", x)), vjust = 0.8, nudge_y = -0.004, color = "black", bg.color = "white", lineheight = 0.8, family = f1, fontface = "bold", size = 3) +
  scale_fill_manual(values = c("#DED7C6", "#1B6C6A"), guide = "none") +
  scale_x_continuous(labels = scales::label_number(drop0trailing = TRUE)) +
  labs(
    title = "Germany speaks the best test English",
    subtitle = "Average overall IELTS Academic score by nationality, on the test's 0–9 scale. Test-takers from countries where English is {.#1B6C6A **official or a primary language**} score high, yet Germany, where it is neither, scores highest of all. Data from 2024–2025.",
    caption = "Data: IELTS · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(family = f1, margin = margin(t = 5), color = "#D98A29", face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_marquee(width = 0.95, lineheight = 1, margin = margin(t = 3, b = 20)),
    plot.caption = element_text(margin = margin(t = 20)), 
    plot.margin = margin(10, 10, 10, 10)
  )
