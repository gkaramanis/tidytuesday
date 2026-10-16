library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

frogs_and_toads_amphibiaweb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-13/frogs_and_toads_amphibiaweb.csv')

# African bullfrog (Pyxicephalus adspersus) by Jose Carlos Arenas-Monroy, PhyloPic, CC0
silhouette_url <- "https://images.phylopic.org/images/23212ae3-9163-44c3-8ee5-a8040e91f0ff/raster/1536x1069.png"
silhouette_file <- tempfile(fileext = ".png")
download.file(silhouette_url, silhouette_file, mode = "wb", quiet = TRUE)
silhouette <- png::readPNG(silhouette_file)
n_channels <- dim(silhouette)[3]

# Alpha channel if there is one (grey + alpha or RGBA), otherwise dark pixels
frog_mask <- if (n_channels %in% c(2, 4)) {
  silhouette[, , n_channels] > 0.5
} else {
  silhouette[, , 1] < 0.5
}

n_species <- nrow(frogs_and_toads_amphibiaweb)

# Hexagonal grid of candidate dot positions, in pixel coordinates
hex_grid <- function(mask, spacing) {
  row_step <- spacing * sqrt(3) / 2
  rows <- seq(1, nrow(mask), by = row_step)

  map(seq_along(rows), \(i) {
    tibble(
      x = seq(1 + (i %% 2) * spacing / 2, ncol(mask), by = spacing),
      row = rows[i]
    )
  }) |>
    list_rbind() |>
    filter(mask[cbind(round(row), round(x))]) |>
    mutate(y = nrow(mask) - row)
}

# Widest spacing that still fits one dot per species
spacings <- seq(20, 5, by = -0.05)
dot_counts <- map_int(spacings, \(s) nrow(hex_grid(frog_mask, s)))
dot_spacing <- spacings[which(dot_counts >= n_species)[1]]

set.seed(42)
dots <- hex_grid(frog_mask, dot_spacing) |>
  slice_sample(n = n_species) |>
  arrange(x, y)

top_families <- frogs_and_toads_amphibiaweb |>
  count(family, sort = TRUE) |>
  slice_head(n = 6)

n_other_families <- n_distinct(frogs_and_toads_amphibiaweb$family) - nrow(top_families)
other_label <- str_glue("{n_other_families} other families")

# Smallest families at the rump, largest at the head
family_order <- c(other_label, rev(top_families$family))

species <- frogs_and_toads_amphibiaweb |>
  mutate(
    family_group = if_else(family %in% top_families$family, family, other_label),
    family_group = factor(family_group, levels = family_order)
  ) |>
  arrange(family_group, genus, specificepithet)

frog_dots <- bind_cols(dots, select(species, family_group, genus))

# Brackets under each stripe, labels staggered on two rows
family_labels <- frog_dots |>
  summarise(
    xmin = min(x) + dot_spacing / 2,
    xmax = max(x) - dot_spacing / 2,
    n = n(),
    .by = family_group
  ) |>
  mutate(
    x = (xmin + xmax) / 2,
    y = if_else(row_number() %% 2 == 0, -150, -50),
    hjust = if_else(family_group == "Rhacophoridae", 0.85, 0.5),
    label = str_glue("{family_group}\n{scales::comma(n)}")
  )

pal <- c(
  "#C9C3B6",
  "#1E9E95", "#2F6DB5", "#D9612B", "#9C4A7C", "#C98E12", "#3B8A4F"
) |>
  set_names(family_order)

top_share <- sum(top_families$n) / n_species
pristimantis_n <- sum(frogs_and_toads_amphibiaweb$genus == "Pristimantis")

f1 <- "Proxima Nova"
f2 <- "Iosevka Charon"
f3 <- "Canela"

bg <- "#F4F0E6"

ggplot(frog_dots, aes(x, y)) +
  geom_point(aes(color = family_group), size = 1.1, stroke = 0) +
  geom_segment(
    data = family_labels,
    aes(x = xmin, xend = xmax, y = -20, yend = -20, color = family_group),
    linewidth = 1.2, lineend = "round"
  ) +
  geom_segment(
    data = family_labels,
    aes(x = x, xend = x, y = -20, yend = y + 10),
    linewidth = 0.25, color = "grey45"
  ) +
  geom_text(
    data = family_labels,
    aes(x = x, y = y, label = label, hjust = hjust),
    family = f2, size = 3.2, lineheight = 0.95, vjust = 1, color = "grey20"
  ) +
  scale_color_manual(values = pal) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.02))) +
  coord_fixed(clip = "off") +
  labs(
    title = "One frog, 8,018 species",
    subtitle = str_glue(
      "Each dot is one species of frog or toad in AmphibiaWeb, grouped by family. ",
      "The six largest families hold {scales::percent(top_share)} of all species, ",
      "and a single genus of rain frogs, *Pristimantis*, accounts for {pristimantis_n} of them."
    ),
    caption = "Source: AmphibiaWeb · Silhouette: Jose Carlos Arenas-Monroy, PhyloPic (CC0) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg, color = NA),
    plot.margin = margin(20, 30, 15, 30),
    plot.title = element_text(family = f3, size = 30, margin = margin(0, 0, 6, 0)),
    plot.subtitle = marquee::element_marquee(size = 13, width = 0.95, lineheight = 1.15, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0, margin = margin(15, 0, 0, 0))
  )

record_polaroid()
