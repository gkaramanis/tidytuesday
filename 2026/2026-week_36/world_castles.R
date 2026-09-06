library(tidyverse)
library(sf)
library(marquee)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.28, height = 6.84, units = "in", dpi = 320)

world_castles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv')

target_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
radius_km <- 100          # Neighborhood for the density count
buffer_km <- 1000         # Buffer for framing
panel_aspect <- 1.2      # Aspect ratio for panels
ref_continent <- "Asia"   # Window size for panels
continents_order <- c( "North America",  "Europe", "Asia", "South America", "Africa", "Oceania")

col_bg <- "#ece7dd"
col_bg2 <- colorspace::lighten(col_bg, amount = 0.8)
col_land <- "#ded7c8"
col_land_focus <- "#b8ad96"
col_border <- "#ece7dd"
col_frame <- "#9c9084"
col_text <- "#453e33"
pal <- MetBrewer::met.brewer("Greek")

f1 <- "Playfair 9pt SemiCondensed"
f2 <- "Gill Sans"

world <- rnaturalearth::ne_countries(scale = "medium") |>
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

world_proj <- st_transform(world, target_crs)

uk <- c("England" = "Europe", "Scotland" = "Europe", "Wales" = "Europe", "Northern Ireland" = "Europe", "Kosovo" = "Europe")

castles <- world_castles |>
  mutate(
    continent = countrycode::countrycode(country, "country.name", "continent", custom_match = uk),
    continent = case_when(
      continent != "Americas" ~ continent,
      countrycode::countrycode(country, "country.name", "region23", warn = FALSE) == "South America" ~ "South America",
      .default = "North America"
    ),
    continent = if_else(country == "Netherlands" & lon < -30, "North America", continent)
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  mutate(neighbors = lengths(st_is_within_distance(geometry, dist = radius_km * 1000))) |>
  st_transform(target_crs) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  arrange(neighbors)

# Same window everywhere, so the continents keep their relative size
half <- castles |>
  filter(continent == ref_continent) |>
  summarise(dx = diff(quantile(X, c(0.02, 0.98))), dy = diff(quantile(Y, c(0.02, 0.98)))) |>
  with(max(dx, dy)) / 2 * 1.1

continent_extent <- function(cont) {
  xy <- filter(castles, continent == cont)

  xy <- filter(xy, abs(X - median(X)) < 1.5 * half, abs(Y - median(Y)) < 1.5 * half)

  reach <- xy |>
    st_as_sf(coords = c("X", "Y"), crs = target_crs) |>
    st_buffer(buffer_km * 1000) |>
    st_union()

  bb <- world_proj |>
    filter(continent == cont) |>
    st_union() |>
    st_intersection(reach) |>
    st_bbox()

  tibble(
    continent = cont,
    cx = (min(xy$X, bb$xmin) + max(xy$X, bb$xmax)) / 2,
    cy = (min(xy$Y, bb$ymin) + max(xy$Y, bb$ymax)) / 2
  )
}

# Manual nudge in km
nudge <- tribble(
  ~continent, ~dx, ~dy,
  "Oceania",  -1850, 500
)

extents <- map(continents_order, continent_extent) |>
  list_rbind() |>
  left_join(nudge, by = "continent") |>
  mutate(
    cx = cx + coalesce(dx, 0) * 1000,
    cy = cy + coalesce(dy, 0) * 1000
  )

totals <- count(castles, continent, name = "total")

top_countries <- castles |>
  count(continent, country) |>
  slice_max(n, n = 3, by = continent, with_ties = FALSE) |>
  summarise(label = paste(paste0("**", country, "**"), n, collapse = "  \n"), .by = continent)

continent_panel <- function(cont) {
  ext <- filter(extents, continent == cont)
  total <- filter(totals, continent == cont)$total
  pad <- 0.05 * half

  ggplot() +
    geom_sf(data = world_proj, fill = col_land, color = col_border, linewidth = 0.15) +
    geom_sf(data = filter(world_proj, continent == cont), fill = col_land_focus, color = col_border, linewidth = 0.15) +
    geom_point(data = filter(castles, continent == cont), aes(X, Y, color = neighbors), size = 0.35) +
    scale_color_gradientn(colors = pal) +
    annotate("marquee", x = ext$cx - half * panel_aspect + pad, y = ext$cy - half + pad,
             label = filter(top_countries, continent == cont)$label,
             hjust = 0, vjust = 0, family = f1, size = 3.5, lineheight = 0.9, color = col_text) +
    coord_sf(xlim = ext$cx + c(-half, half) * panel_aspect, ylim = ext$cy + c(-half, half), expand = FALSE) +
    labs(title = paste0(paste0("**", cont, "**"), "  ", format(total, big.mark = ","))) +
    theme_void(base_family = f1)
}

# Plot
wrap_plots(map(continents_order, continent_panel), ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "**Castles of the World**",
    subtitle = "Every dot is one of the 5,793 castles, fortresses, palaces and ruins that Castlemap gathers from Wikidata, each one with\nverified coordinates, a Wikipedia article and a photo on Wikimedia Commons. Color shows how many others stand within 100 km\n(brighter is more), the number beside each continent is its total, and the three countries with the most landmarks are named.",
    caption = "Source: Wikidata via Castlemap · Graphic: Georgios Karamanis",
    theme = theme(
      plot.background = element_rect(fill = col_bg, color = NA),
      plot.title = element_text(family = f2, size = 16),
      plot.subtitle = element_marquee(family = f2, size = 12, width = 1),
      plot.caption = element_text(family = f2)
    )
  ) &
  theme(
    legend.position = "none",
    text = element_text(color = col_text, family = f1),
    plot.title = element_marquee(hjust = 0.5, margin = margin(b = 2)),
    panel.border = element_rect(color = col_frame, fill = NA, linewidth = 0.3),
    panel.background = element_rect(fill = col_bg2, color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )