library(tidyverse)
library(tidytext)
library(SnowballC)
library(ggraph)
library(tidygraph)
library(ggstar)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8.5, units = "in", dpi = 320)

apod <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-20/apod.csv')

apod_clean <- apod |> 
  mutate(explanation = str_remove(explanation, " +digg_url.+digg_skin.+$"))

apod_stems <- apod_clean |> 
  unnest_tokens(word, explanation) |> 
  anti_join(stop_words) |> 
  mutate(stem = wordStem(word, language = "english"))

top_stems <- apod_stems |> 
  add_count(stem) |> 
  distinct(stem, n) |> 
  slice_max(order_by = n, n = 50)

top_stems_words <- apod_stems |> 
  count(stem, word) |> 
  filter(stem %in% top_stems$stem) |> 
  distinct(stem, word, n) |> 
  arrange(stem, n) |> 
  group_by(stem) |> 
  mutate(rank = row_number())

nodes <- bind_rows(
  top_stems |> select(name = stem, n) |> mutate(name = paste0("stem:", name)),
  top_stems_words |> select(name = word, n) |> distinct()
) |> 
  mutate(size_group = cut(n, breaks = c(0, 1000, 3000, 6000, Inf), include.lowest = TRUE, labels = c("Low (<1k)", "Moderate (1k–3k)", "High (3k–6k)", "Very High (6k+)")))

edges <- top_stems_words |> 
  select(from = stem, to = word) |> 
  mutate(from = paste0("stem:", from))

graph <- tbl_graph(nodes = nodes, edges = edges)

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Graphik Compact"

col_blue <- "#2A254F"
col_yellow <- "#FFED4E"
col_gold <- "#E3BF0E"
col_grey <- "#8C889E"

set.seed(29979245)

ggraph(graph, layout = 'circlepack', weight = n) +
  geom_node_circle(linewidth = 0.07, color = col_grey) +
  geom_edge_link(color = col_gold, linewidth = 0.3) +
  geom_star(data = . %>% filter(str_detect(name, "stem", negate = TRUE)), aes(x, y, size = size_group, starshape = size_group), fill = col_yellow, color = col_gold) +
  geom_node_point(data = . %>% filter(str_detect(name, "stem")), size = 1.5, color = col_gold) +
  ggrepel::geom_text_repel(data = . %>% filter(str_detect(name, "stem", negate = TRUE) & n < 1000), aes(x, y, label = name, size = size_group), family = f1, segment.size = 0.1, bg.color = col_blue, color = col_grey, seed = 9, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = . %>% filter(str_detect(name, "stem", negate = TRUE) & n >= 1000), aes(x, y, label = name, size = size_group), family = f1, segment.size = 0.1, bg.color = col_blue, color = "white", seed = 9, show.legend = FALSE) +
  scale_size_manual(name = "Count ranges", values = c(2, 3, 5, 7)) +
  scale_starshape_manual(name = "Count ranges", values = c(22, 14, 1, 27)) +
  # coord_fixed() +
  labs(
    title = "APOD is made of stars and light",
    subtitle = str_wrap("NASA's Astronomy Picture of the Day (APOD) shares a cosmic image every day, each with detailed scientific explanations. By analyzing these description texts, removing common words and grouping related terms, we see which concepts appear most often. The 50 most common word stems appear as points, with larger stars representing individual words clustered around them.", 125),
    caption = "Source: NASA Astronomy Picture of the Day (APOD) archive · Graphic: Georgios Karamanis"
  ) +
  guides(
    size = guide_legend(nrow = 1),
    starshape = guide_legend(nrow = 1)
  ) +
  theme_void(base_family = f2, ink = "grey99") +
  theme(
    text = element_text(face = "italic"),
    legend.position = "top",
    legend.text = element_text(face = "italic", margin = margin(r = 10, l = 3)),
    plot.background = element_rect(fill = col_blue, color = NA),
    plot.title = element_text(face = "bold.italic", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, margin = margin(b = 20)),
    plot.margin = margin(10, 20, 10, 20)
  )

record_polaroid()
