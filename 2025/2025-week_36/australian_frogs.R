library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("tidytuesday-temp/"), width = 12, height = 8, dpi = 320)

frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')

frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')

frogs <- frogID_data |>
  left_join(frog_names) |>
  filter(!is.na(tribe)) |>
  mutate(
    month = month(eventDate),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Autumn",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring",
      TRUE ~ NA
    ),
  ) |>
  add_count(tribe, season) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

common_frogs <- frogs |> 
  st_drop_geometry() |> 
  count(tribe, commonName) |> 
  group_by(tribe) |>
  slice_max(order_by = n, n = 3) |> 
  ungroup() |> 
  group_by(tribe) |> 
  reframe(label = paste0("**", tribe, "**  \n", 
                         str_wrap(str_c(commonName, collapse = ", "), 10)
                         )) |> 
  distinct()

hex_grid <- st_make_grid(frogs, square = FALSE, n = c(30, 30))
hex_sf <- st_sf(hex_id = seq_along(hex_grid), geometry = hex_grid)

hex_counts <- frogs |>
  st_join(hex_sf) |>
  st_drop_geometry() |>
  count(tribe, season, hex_id, name = "n") |>
  left_join(hex_sf, by = "hex_id") |>
  mutate(
    centroid = st_centroid(geometry),
    x = st_coordinates(centroid)[, 1],
    y = st_coordinates(centroid)[, 2]
  ) |> 
  left_join(common_frogs) |> 
  `st_geometry<-`("geometry")

aus <- rnaturalearth::ne_countries(country = "Australia")

f1 <- "Bricolage Grotesque 12pt"
f2 <- "Bricolage Grotesque 12pt Condensed"

ggplot() +
  geom_sf(data = aus, color = NA, fill = "grey90") +
  geom_sf(data = hex_counts, aes(fill = n), color = "grey99", linewidth = 0.1) +
  ggrepel::geom_text_repel(data = hex_counts |> filter(n == max(n), .by = c(season, tribe)), aes(geometry = geometry, label = format(n, big.mark = " ")), color = "black", size = 3.5, fontface = "bold", family = f2, stat = "sf_coordinates", bg.color = "white", seed = 99) +
  MetBrewer::scale_fill_met_c("Tam") +
  facet_grid(vars(season), vars(label)) +
  labs(
    title = "Australian frog observations by tribe and season",
    subtitle = "Number of frog observations recorded in each hexagonal grid cell. Labels show the maximum count per tribe and season.",
    caption = "Source: FrogID · Graphic: Georgios Karamanis"
  ) + 
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "line"),
    legend.key.height = unit(0.3, "line"),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey95", color = NA),
    strip.text = ggtext::element_markdown(size = 5, margin = margin(b = 5)),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )
