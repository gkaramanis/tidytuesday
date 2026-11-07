library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggfittext)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

grambank_languages <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-03/grambank_languages.csv")
# grambank_parameters <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-03/grambank_parameters.csv")
grambank_values <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-03/grambank_values.csv")

glottolog <- read_csv(
  "https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv",
  col_select = c(ID, Name)
)

# Drop proto-languages, creoles and pidgins
languages <- grambank_languages |>
  filter(
    !is.na(macroarea),
    family_name == "Indo-European",
    !str_detect(lineage, "guin1259|earl1243"),
    !glottocode %in% c("hait1244", "berb1259")
  ) |>
  left_join(
    grambank_values |>
      filter(parameter_id == "GB030") |>
      select(glottocode = language_id, value),
    by = join_by(glottocode)
  ) |>
  mutate(
    # Grambank codes Lithuanian 0, but it is gendered
    value = if_else(glottocode == "lith1251", 1, value),
    gender_3p = recode_values(value, 1 ~ "Yes", 0 ~ "No")
  )

ancestors <- languages$lineage |>
  str_split("/") |>
  unlist() |>
  unique()

# Languages that are also ancestors get their own tip
paths <- languages |>
  mutate(
    is_ancestor = glottocode %in% ancestors,
    tip_label = if_else(is_ancestor, str_c(glottocode, "_self"), glottocode),
    tip_path = if_else(is_ancestor, str_c(glottocode, "/", tip_label), tip_label),
    path = str_c(lineage, "/", tip_path)
  )

# Remove single-child nodes
collapse_singles <- function(edges) {
  repeat {
    singles <- edges |>
      count(parent) |>
      filter(n == 1) |>
      pull(parent)
    if (length(singles) == 0) {
      return(edges)
    }
    single <- singles[1]
    only_child <- edges$child[edges$parent == single]
    edges <- edges |>
      filter(parent != single) |>
      mutate(child = replace(child, child == single, only_child))
  }
}

edges <- str_split(paths$path, "/") |>
  map(\(nodes) tibble(parent = head(nodes, -1), child = nodes[-1])) |>
  list_rbind() |>
  distinct() |>
  collapse_singles()

nodes <- tibble(name = unique(c(edges$parent, edges$child))) |>
  left_join(
    select(paths, tip_label, language_name, gender_3p, longitude),
    by = join_by(name == tip_label)
  ) |>
  left_join(glottolog, by = join_by(name == ID)) |>
  mutate(label = coalesce(language_name, Name))

graph <- tbl_graph(
  nodes = nodes,
  edges = rename(edges, from = parent, to = child)
) |>
  # Share of descendant languages with gendered pronouns
  mutate(share_gender = map_local_dbl(
    order = graph_order(),
    mode = "out",
    .f = \(neighborhood, ...) {
      vals <- as_tibble(neighborhood, active = "nodes")$gender_3p
      if (all(is.na(vals))) NA_real_ else mean(vals == "Yes", na.rm = TRUE)
    }
  ),
  # Mean longitude of descendant languages, for west-to-east ordering
  mean_longitude = map_local_dbl(
    order = graph_order(),
    mode = "out",
    .f = \(neighborhood, ...) {
      mean(as_tibble(neighborhood, active = "nodes")$longitude, na.rm = TRUE)
    }
  ))

circle_start <- (180 + 30) * pi / 180

# Tile bounds, root ring at radius 0
sunburst <- graph |>
  # Node order sets sibling order in the partition layout
  arrange(mean_longitude) |>
  create_layout(layout = "partition") |>
  as_tibble() |>
  mutate(
    xmin = x - width / 2,
    xmax = x + width / 2,
    ymin = y - height / 2 - 1,
    ymax = y + height / 2 - 1,
    theta = (360 * x / max(xmax) + circle_start * 180 / pi) %% 360,
    angle = 90 - theta,
    flip = theta > 180,
    label_angle = if_else(flip, angle + 180, angle),
    label_hjust = if_else(flip, 1, 0)
  )

f1 <- "Bricolage Grotesque 12pt Condensed"
f2 <- "Newsreader Text"

# Main branches: ring-2 nodes plus Hieroglyphic Luwian
branch_gaps <- sunburst |>
  filter(depth == 2 | (depth == 1 & leaf)) |>
  transmute(x = xmin, y = if_else(xmin == 0, 1, ymin), yend = max(sunburst$ymax))

sunburst_plot <- ggplot(sunburst) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = share_gender), colour = "grey99", linewidth = 0.4) +
  geom_segment(data = branch_gaps, aes(x = x, xend = x, y = y, yend = yend), colour = "grey99", linewidth = 1.5) +
  geom_fit_text(data = sunburst |> filter(!leaf) |> mutate(xmax = if_else(depth == 0, xmax - 0.01, xmax)), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label), size = 12, family = f1, min.size = 5, flip = TRUE) +
  geom_text(data = filter(sunburst, leaf), aes(x = x, y = ymax + 0.3, label = label, angle = label_angle, hjust = label_hjust), size = 4) +
  scale_fill_gradient2(low = "#C99A2E", mid = "grey93", high = "#9B6BC4", midpoint = 0.5, na.value = "pink", breaks = c(0, 0.5, 1), labels = scales::label_percent(), name = "Share of languages in the branch\nwith gendered third-person pronouns") +
  scale_y_continuous(limits = c(-1, NA)) +
  coord_polar(start = circle_start, clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(2, "lines"),
    legend.title = element_text(size = 13, hjust = 0.5, margin = margin(b = 8)),
    legend.text = element_text(size = 12),
    legend.justification = c(0, 1),
    legend.margin = margin(50, 0, 0, 50),
    plot.margin = margin(-40, -20, -40, -20)
  )

text_style <- marquee::classic_style(base_size = 10.5, body_font = f2, header_font = f2, lineheight = 1.3) |>
  marquee::modify_style("base", lineheight = 1.35) |>
  marquee::modify_style("h1", size = marquee::relative(1.8), weight = "bold", lineheight = 1.1, border = "transparent", border_size = marquee::trbl(0), padding = marquee::trbl(0), margin = marquee::trbl(0, 0, marquee::em(0.5), 0))

text_panel <- ggplot() +
  labs(
    title = "# Europe says he and she. Asia often doesn’t.\n\nShare of Indo-European languages whose third-person pronouns mark gender, as English does with *he*, *she* and *it*.\n\nEach ring is one level of the family tree, from Indo-European at the centre to single languages at the edge. Wider gaps separate the main branches, which are ordered clockwise by the average longitude of their languages, from Germanic at the lower left to Indo-Iranian at the lower right.\n\nIn the European branches, 38 of 39 languages (97%) are {#7B4FA6 **gendered**}. In the Asian branches (Indo-Iranian, Armenian and Anatolian), 17 of 25 (68%) are {#94701A **not**}. Neither Armenian language marks gender, only one of five Iranian languages does, and Indo-Aryan splits almost evenly, 7 of 16.\n\nCreoles and pidgins are excluded. Grambank doesn’t code Central Kurdish for this feature."
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.title = marquee::element_marquee(style = text_style, width = 1, lineheight = 1.2),
    plot.margin = margin(40, 0, 0, 30)
  )

wrap_elements(full = text_panel) + guide_area() + sunburst_plot +
  plot_layout(
    design = "AC\nBC",
    widths = c(1.25, 2.75),
    heights = c(3.1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    caption = "Source: Grambank, Glottolog · Graphic: Georgios Karamanis",
    theme = theme(
      legend.justification = c(0, 1),
      plot.caption = element_text(family = f1, size = 11, hjust = 0, colour = "grey30", margin = margin(l = 22, b = 15))
    )
  ) &
  theme(plot.background = element_rect(fill = "grey99", color = NA))
