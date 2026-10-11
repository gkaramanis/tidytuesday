library(tidyverse)
library(ggsankey)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

avocado_oil_bottles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-06/avocado_oil_bottles.csv')
avocado_oil_processed_foods <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-06/avocado_oil_processed_foods.csv')

# value keeps each product's authenticity on every axis
oil_long <- avocado_oil_processed_foods |>
  transmute(
    category = str_to_title(str_replace(category, "_", "\n")),
    oil_type = str_to_title(paste(oil_type, "\nOil")),
    authentic = if_else(authentic, "Authentic", "Not\nauthentic"),
    auth = authentic
  ) |>
  make_long(category, oil_type, authentic, value = auth) |>
  mutate(across(c(node, next_node), \(n) fct_relevel(n, "Authentic", after = Inf)))

# Share of authentic products per node and per flow
node_share <- oil_long |>
  summarise(p_auth = mean(value == "Authentic"), .by = node)

flow_share <- oil_long |>
  filter(!is.na(next_node)) |>
  summarise(p_auth = mean(value == "Authentic"), .by = c(node, next_node))

# Sankey geometry from ggsankey: flows (polygons), nodes (rects), labels
p <- ggplot(oil_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = node)) +
  geom_sankey() +
  geom_sankey_text()

b <- ggplot_build(p)$data

# Nodes: split each rect at its authentic share
nodes <- b[[2]] |>
  select(x, node, xmin, xmax, ymin, ymax) |>
  left_join(node_share, by = join_by(node)) |>
  mutate(y_split = ymax - p_auth * (ymax - ymin))

# Flows: label is the start node; the end node is the rect that holds the flow's right end
flow_ids <- b[[1]] |>
  distinct(group, node = label, x_end = round(xmin_end), flow_end_ymin, flow_end_ymax) |>
  left_join(
    nodes |> select(x_end = x, next_node = node, ymin, ymax),
    by = join_by(x_end, flow_end_ymin >= ymin, flow_end_ymax <= ymax)
  ) |>
  left_join(flow_share, by = join_by(node, next_node))

# Each polygon is the top edge left to right, then the bottom edge right to left
flows <- b[[1]] |>
  reframe(
    y_top = head(y, length(y) / 2),
    y_bottom = rev(tail(y, length(y) / 2)),
    x = head(x, length(x) / 2),
    .by = group
  ) |>
  left_join(flow_ids |> select(group, p_auth), by = join_by(group)) |>
  mutate(y_split = y_top - p_auth * (y_top - y_bottom))

f1 <- "Iosevka Charon"
f2 <- "Proxima Nova"

col_auth <- "#1B9E77"
col_not <- "#BE631D"

# Big number for avocado oil (not authentic), small one for olive oil (authentic)
oil_labels <- nodes |>
  filter(x == 2) |>
  mutate(
    avocado = str_detect(node, "Avocado"),
    label = if_else(avocado, scales::percent(1 - p_auth), scales::percent(p_auth)),
    y = if_else(avocado, (ymin + y_split) / 2, (y_split + ymax) / 2),
    col = if_else(avocado, col_not, col_auth),
    size = if_else(avocado, 16, 6)
  )

ggplot() +
  geom_ribbon(data = flows, aes(x = x, ymin = y_split, ymax = y_top, group = group), fill = col_auth, alpha = 0.8) +
  geom_ribbon(data = flows, aes(x = x, ymin = y_bottom, ymax = y_split, group = group), fill = col_not, alpha = 0.8) +
  geom_rect(data = nodes, aes(xmin = xmin, xmax = xmax, ymin = y_split, ymax = ymax), fill = col_auth, color = "white", linewidth = 1) +
  geom_rect(data = nodes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = y_split), fill = col_not, color = "white", linewidth = 1) +
  shadowtext::geom_shadowtext(data = b[[3]], aes(x = x, y = y, label = label), family = f1, size = 7) +
  geom_text(data = oil_labels, aes(x = xmax + 0.3, y = y, label = label, color = col, size = size), hjust = 0, family = f1, color = "grey99") +
  scale_color_identity() +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = "Nearly 9 in 10 \"avocado oil\" foods aren't avocado oil",
    subtitle = str_c(
      "In a 2026 UC Davis study, 48 of the 54 chips, mayonnaises and salad dressings labelled \"made with avocado oil\" were ",
      "{", col_not, " **not authentic**}. Of the 20 products made with olive oil, 19 were {", col_auth, " **authentic**}."
    ),
    caption = "Source: UC Davis Department of Food Science and Technology · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 24, face = "bold", family = f2),
    plot.subtitle = marquee::element_marquee(size = 14, width = 0.9, margin = margin(5, 0, 20, 0), family = f2, lineheight = 1.1),
    plot.caption = element_text(size = 10, family = f2, color = "grey20", hjust = 0, margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )
