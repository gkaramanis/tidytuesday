library(tidyverse)
library(ggimage)
library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 8, height = 9, dpi = 320)

# Read FIDE ratings data for August and September
fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

# Piece positions for each month
piece_positions <- tibble::tribble(
  ~rank, ~piece, ~"September", ~"August",
  1, "k", "e1", "e8",
  2, "q", "d1", "d8",
  3, "r", "a1", "a8",
  4, "r", "h1", "h8",
  5, "b", "c1", "c8",
  6, "b", "f1", "f8",
  7, "n", "b1", "b8",
  8, "n", "g1", "g8",
  9, "p", "a2", "a7",
  10, "p", "b2", "b7",
  11, "p", "c2", "c7",
  12, "p", "d2", "d7",
  13, "p", "e2", "e7",
  14, "p", "f2", "f7",
  15, "p", "g2", "g7",
  16, "p", "h2", "h7"
) |>
  pivot_longer(September:August, names_to = "month", values_to = "position")

# Combine ratings, select top 16, join piece positions, and prepare labels
fide_ratings <- bind_rows(
  fide_ratings_august |> mutate(month = "August"),
  fide_ratings_september |> mutate(month = "September")
) |>
  group_by(month) |>
  slice_max(order_by = rating, n = 16) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  left_join(piece_positions) |>
  mutate(
    x = str_sub(position, 1, 1),
    y = as.numeric(str_sub(position, 2, 2)),
    label = paste0(rank, ". ", name, "\n", scales::number(rating)),
    color = if_else(y < 5, "white", "black"),
    img = paste0(
      "https://raw.githubusercontent.com/lichess-org/lila/refs/heads/master/public/piece/monarchy/",
      str_sub(color, 1, 1), toupper(piece), ".svg"
    )
  )

# Fonts and colors
f1 <- "Bricolage Grotesque 12pt Condensed"
f2 <- "Hoefler Text"
cb_cols <- c("#f0d9b5", "#b58863")
bg_col <- "#f7f7f7"
ln_col <- "#8e2442"
lb_col <- "#444444"

ratio = 0.16

ggplot(fide_ratings, aes(x = month, y = rank)) +
  ggforce::geom_diagonal(data = fide_ratings |> select(name, month, rank) |> pivot_wider(names_from = month, values_from = rank), aes(x = "August", xend = "September", y = August, yend = September, group = name), alpha = 0.3, color = ln_col) +
  geom_tile(aes(fill = as.factor(rank %% 2 == 0)), color = bg_col, height = 1, width = ratio, linewidth = 1) +
  geom_image(aes(image = img), size = 0.05) +
  geom_text(aes(label = label, hjust = if_else(month == "August", 1, 0), nudge_x = if_else(month == "August", -0.11, 0.11)), size = 4.2, family = f1, lineheight = 0.95, color = lb_col) +
  scale_x_discrete(expand = expansion(0.769), sec.axis = dup_axis()) +
  scale_y_reverse(expand = 0) +
  scale_fill_manual(values = cb_cols) +
  coord_fixed(ratio = ratio, clip = "off") +
  labs(
    title = "FIDE Chess Player Ratings",
    caption = "Source: FIDE · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    axis.text.x.top = element_text(margin = margin(0, 0, 8, 0), face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(hjust = 0.5, size = 10)
  )

# Manually save until camcorder gets updated for ggplot2 4.0.0
record_polaroid()