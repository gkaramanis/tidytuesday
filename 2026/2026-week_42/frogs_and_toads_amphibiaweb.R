library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-13/frogs_and_toads_amphibiaweb.csv')

# African bullfrog (Pyxicephalus adspersus) by Jose Carlos Arenas-Monroy, PhyloPic, CC0
silhouette <- png::readPNG(curl::curl_fetch_memory("https://images.phylopic.org/images/23212ae3-9163-44c3-8ee5-a8040e91f0ff/raster/1536x1069.png")$content)
frog_mask <- silhouette[, , dim(silhouette)[3]] > 0.5

species_per_dot <- 5
n_species <- nrow(frogs)
n_dots <- ceiling(n_species / species_per_dot)

# Hexagonal grid of candidate dot positions, in pixel coordinates
hex_grid <- function(spacing) {
  expand_grid(i = 0:(nrow(frog_mask) %/% (spacing * sqrt(3) / 2)), j = 0:(ncol(frog_mask) %/% spacing)) |>
    mutate(row = 1 + i * spacing * sqrt(3) / 2, x = 1 + j * spacing + ((i + 1) %% 2) * spacing / 2) |>
    filter(row <= nrow(frog_mask), x <= ncol(frog_mask)) |>
    filter(frog_mask[cbind(round(row), round(x))]) |>
    mutate(y = nrow(frog_mask) - row)
}

# Widest spacing that still fits all the dots
dot_spacing <- detect(seq(60, 10, by = -0.05), \(s) nrow(hex_grid(s)) >= n_dots)

# Trim the surplus from the outline, so the inside has no gaps
dots <- hex_grid(dot_spacing) |>
  mutate(neighbours = rowSums(as.matrix(dist(pick(x, y))) < dot_spacing * 1.1)) |>
  slice_max(neighbours, n = n_dots, with_ties = FALSE) |>
  arrange(x, y)

other_label <- str_c(n_distinct(frogs$family) - 6, " other families")

# Smallest families at the rump, largest at the head
species <- frogs |>
  mutate(family_group = fct_lump_n(family, 6, other_level = other_label) |> fct_infreq() |> fct_rev() |> fct_relevel(other_label)) |>
  arrange(family_group, genus, specificepithet)

# Each dot takes the family of every 5th species in the sorted list
frog_dots <- dots |>
  mutate(family_group = species$family_group[seq(1, n_species, by = species_per_dot)])

# Brackets under each stripe, labels staggered on two rows
family_labels <- frog_dots |>
  summarise(xmin = min(x) + dot_spacing / 2, xmax = max(x) - dot_spacing / 2, .by = family_group) |>
  left_join(count(species, family_group), by = join_by(family_group)) |>
  mutate(
    x = (xmin + xmax) / 2,
    y = if_else(row_number() %% 2 == 0, -150, -50),
    hjust = if_else(family_group == "Rhacophoridae", 0.85, 0.5),
    label = str_glue("{family_group}\n{scales::comma(n)}")
  )

top_share <- mean(species$family_group != other_label)
pristimantis_n <- sum(frogs$genus == "Pristimantis")

f1 <- "Proxima Nova"
f2 <- "Iosevka Charon"
f3 <- "Canela"

ggplot() +
  geom_point(data = frog_dots, aes(x, y, color = family_group), size = dot_spacing * 0.13, stroke = 0) +
  geom_segment(data = family_labels, aes(x = xmin, xend = xmax, y = -45, yend = -45, color = family_group), linewidth = 1.5, lineend = "round") +
  geom_segment(data = family_labels, aes(x = x, xend = x, y = -50, yend = y - 15), linewidth = 0.25, color = "grey45") +
  geom_text(data = family_labels, aes(x, y - 30, label = label, hjust = hjust), family = f2, size = 3.2, lineheight = 0.95, vjust = 1, color = "grey20") +
  scale_color_manual(values = c("#C9C3B6", "#1E9E95", "#2F6DB5", "#D9612B", "#9C4A7C", "#C98E12", "#3B8A4F")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.01))) +
  coord_fixed(clip = "off") +
  labs(
    title = str_glue("One frog, {scales::comma(n_species)} species"),
    subtitle = str_glue(
      "Each dot is {species_per_dot} species of frog or toad in AmphibiaWeb, grouped by family. ",
      "The six largest families hold {scales::percent(top_share)} of all species, ",
      "and a single genus of rain frogs, *Pristimantis*, accounts for {pristimantis_n} of them."
    ),
    caption = "Source: AmphibiaWeb · Silhouette: Jose Carlos Arenas-Monroy, PhyloPic (CC0) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#F4F0E6", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = f3, size = 26, margin = margin(0, 0, 6, 0)),
    plot.subtitle = marquee::element_marquee(size = 13, width = 0.98, lineheight = 1.15, margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0, margin = margin(15, 0, 0, 0))
  )

record_polaroid()
