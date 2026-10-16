library(tidyverse)
library(camcorder)
library(ggvmap)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

frogs_and_toads_amphibiaweb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-13/frogs_and_toads_amphibiaweb.csv')

# Silhouette -------------------------------------------------------------

# African bullfrog (Pyxicephalus adspersus) by Jose Carlos Arenas-Monroy, PhyloPic, CC0.
# For an SVG outline instead: silhouette <- magick::image_read_svg("frog.svg", width = 400)
silhouette_url <- "https://images.phylopic.org/images/23212ae3-9163-44c3-8ee5-a8040e91f0ff/raster/1536x1069.png"
silhouette <- magick::image_read(silhouette_url) |>
  magick::image_resize("400x")

frog_mask <- (as.integer(magick::image_data(silhouette, "rgba"))[, , 4] > 127) * 1

# Trace the mask, keep the main body, and smooth pixel steps out of the edge
frog_outline <- isoband::isobands(
  seq_len(ncol(frog_mask)), rev(seq_len(nrow(frog_mask))), frog_mask, 0.5, 1.5
) |>
  isoband::iso_to_sfg() |>
  pluck(1) |>
  sf::st_sfc() |>
  sf::st_make_valid() |>
  sf::st_cast("POLYGON") |>
  (\(p) p[which.max(sf::st_area(p))])() |>
  sf::st_simplify(dTolerance = 1.5) |>
  sf::st_buffer(1) |>
  sf::st_buffer(-1)

# Families ---------------------------------------------------------------

# isocc includes introduced countries, so remove those to get native ranges
species_continents <- frogs_and_toads_amphibiaweb |>
  mutate(
    native = map2(
      str_extract_all(isocc, "[A-Z]{2}"),
      str_extract_all(coalesce(intro_isocc, ""), "[A-Z]{2}"),
      setdiff
    )
  ) |>
  select(amphib_id, family, native) |>
  unnest(native) |>
  mutate(continent = countrycode::countrycode(native, "iso2c", "continent", warn = FALSE)) |>
  distinct(amphib_id, family, continent) |>
  filter(!is.na(continent))

# The continent where most of a family's species live; a species on two continents counts for both
main_continent <- species_continents |>
  count(family, continent) |>
  slice_max(n, n = 1, by = family, with_ties = FALSE) |>
  select(family, continent)

families <- frogs_and_toads_amphibiaweb |>
  count(family) |>
  left_join(main_continent, by = join_by(family), relationship = "one-to-one") |>
  mutate(continent = factor(continent, levels = c("Americas", "Asia", "Africa", "Oceania", "Europe"))) |>
  arrange(-n)

continent_totals <- families |>
  summarise(families = n(), species = sum(n), .by = continent) |>
  mutate(share = species / sum(species))

americas <- continent_totals |> filter(continent == "Americas")

vm <- vmap_region(
  families$n,
  sf::st_coordinates(frog_outline)[, 1:2],
  labels = families$family,
  seed = 1
)

# Area must match the data before plotting
stopifnot(vm$converged)

cells <- vm_as_df(vm) |>
  left_join(families, by = join_by(label == family), relationship = "many-to-one")

cell_labels <- vm_centroids(vm) |>
  left_join(families, by = join_by(label == family), relationship = "many-to-one") |>
  slice_max(n, n = 14) |>
  mutate(
    label = str_glue("{label}\n{scales::comma(n)}"),
    text_col = "grey97"
  )

# Plot -------------------------------------------------------------------

f1 <- "Proxima Nova"
f2 <- "Iosevka Charon"
f3 <- "Canela"

bg <- "#F4F0E6"

ggplot() +
  geom_polygon(
    data = cells,
    aes(x, y, group = cell, subgroup = ring, fill = continent),
    rule = "evenodd", color = bg, linewidth = 0.6
  ) +
  geom_text(
    data = cell_labels,
    aes(cx, cy, label = label, color = text_col),
    family = f2, size = 3, lineheight = 0.9
  ) +
  scale_fill_manual(
    # Lynch "mulholland_drive": blue, navy, tan, red, slate
    values = Lynch::lynch_palette("mulholland_drive") |>
      as.character() |>
      set_names(c("Americas", "Europe", "Africa", "Asia", "Oceania")),
    drop = TRUE,
    name = "Continent where most of the family's species live",
    guide = guide_legend(nrow = 1, keywidth = unit(3, "lines"), keyheight = unit(0.6, "lines"))
  ) +
  scale_color_identity() +
  coord_fixed(clip = "off") +
  labs(
    title = "A frog of 57 families",
    subtitle = str_glue(
      "Each cell is one family of frogs and toads, sized by its number of species. ",
      "Colour marks the continent where most of a family's species live. ",
      "{americas$families} families are mainly American, and together they hold ",
      "{scales::percent(americas$share)} of all frog species."
    ),
    caption = "Source: AmphibiaWeb · Silhouette: Jose Carlos Arenas-Monroy, PhyloPic (CC0) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 10, color = "grey25"),
    legend.title.position = "top",
    legend.text = element_text(family = f2, size = 9, color = "grey25"),
    legend.text.position = "bottom",
    legend.key.spacing.x = unit(2, "pt"),
    legend.margin = margin(20, 0, 10, 0),
    plot.background = element_rect(fill = bg, color = NA),
    plot.margin = margin(20, 30, 15, 30),
    plot.title = element_text(family = f3, size = 30, margin = margin(0, 0, 6, 0)),
    plot.subtitle = marquee::element_marquee(size = 13, width = 0.95, lineheight = 1.15, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0, margin = margin(15, 0, 0, 0))
  )

record_polaroid()
