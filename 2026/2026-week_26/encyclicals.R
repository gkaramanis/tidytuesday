library(tidyverse)
library(tidytext)
library(lofifonts)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 6, height = 8, units = "in", dpi = 320)

encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/encyclicals.csv')
# papal_encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/papal_encyclicals.csv')
# scripture_references <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/scripture_references.csv')

en_rates <- encyclicals |> 
  unnest_tokens(word, text) |> 
  count(encyclical, word, sort = TRUE) |> 
  filter(!word %in% stop_words$word) |> 
  group_by(encyclical) |>
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  pivot_wider(id_cols = word, names_from = encyclical, values_from = prop, values_fill = 0) |> 
  mutate(prop_diff = `Magnifica Humanitas` - `Rerum Novarum`)

top_diff <- bind_rows(
  slice_max(en_rates, prop_diff, n = 10),
  slice_min(en_rates, prop_diff, n = 10)
) |>
  mutate(favored = if_else(prop_diff > 0, "Magnifica Humanitas", "Rerum Novarum"))

k <- 7000

td_lofi <- top_diff |>
  mutate(coords = map(as.character(word), vector_text_coords)) |>
  arrange(desc(prop_diff)) |>
  mutate(
    word_idx = row_number(),
    width = prop_diff * k,
    bar_len = abs(width)
  ) |>
  unnest(coords) |>
  group_by(word_idx) |>
  mutate(x2 = scales::rescale(x, to = sort(c(0, width[1])))) |>
  ungroup() |>
  mutate(y2 = y + word_idx * 9)

enc_info <- encyclicals |>
  distinct(encyclical, pope, year) |>
  rename(favored = encyclical)

annot <- td_lofi |>
  group_by(favored) |>
  summarise(y = mean(range(y2)), .groups = "drop") |>
  left_join(enc_info, by = "favored") |>
  mutate(
    x = if_else(favored == "Magnifica Humanitas", min(td_lofi$x2), max(td_lofi$x2)),
    hjust = if_else(favored == "Magnifica Humanitas", 0, 1),
    label = paste0(favored, "\n", pope, " · ", year)
  )

f1 <- "Rowan"

ggplot(td_lofi) +
  geom_path(aes(x2, y2, group = interaction(word, char_idx, stroke_idx), color = favored, linewidth = bar_len), lineend = "round") +
  geom_text(data = annot, aes(x, y, label = label, color = favored, hjust = hjust), fontface = "bold", size = 5, lineheight = 0.9) +
  scale_linewidth_continuous(range = c(0.5, 1)) +
  scale_color_manual(values = c("Magnifica Humanitas" = "#15616d", "Rerum Novarum" = "#9e2a2b")) +
  scale_x_continuous(
    breaks = \(lims) scales::breaks_width(0.002)(lims / k) * k,
    labels = \(x) scales::percent(abs(x) / k, accuracy = 0.1),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Two Popes Named Leo, 135 Years Apart",
    subtitle = str_wrap("Leo XIII wrote on the rights of labor. Leo XIV on human dignity in the age of AI. The 20 words with the widest gap in frequency between the two texts.", 76),
    caption = "Source:  Vatican.va · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f4f1e9", color = NA),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.15),
    axis.text.x = element_text(size = 9, margin = margin(t = 15), color = c("grey30", "grey80")),
    plot.title = element_text(face = "bold", size = 20, margin = margin(b = 7), hjust = 0.5),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20), hjust = 0.5, lineheight = 1),
    plot.caption = element_text(color = "grey50", size = 8, margin = margin(t = 12)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 20, 10, 20)
  )

record_polaroid()
