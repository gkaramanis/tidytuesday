library(tidyverse)
library(tidytext)
library(ggbump)
library(gghighlight)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 6, units = "in", dpi = 320)

country_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/country_lyrics.csv')
# top_all_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_all_writers.csv')
# top_primary_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_primary_writers.csv')
# top_producers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-25/top_producers.csv')

custom_stop_words <- tribble(
  ~word, ~lexicon,
  "yeah", "custom",
  "wanna", "custom",
  "gonna", "custom",
  "chorus", "custom",
  "verse", "custom",
  "outro", "custom",
  "intro", "custom",
  "refrain", "custom",
  "repeat", "custom"
)

pal <- MetBrewer::met.brewer("Juarez", direction = -1)

top_words <- country_lyrics |>
  unnest_tokens(word, lyrics) |>
  anti_join(stop_words, by = "word") |>
  anti_join(custom_stop_words, by = "word") |>
  distinct(entered_top_30_in, rough_order, word) |>
  count(entered_top_30_in, word) |>
  left_join(count(country_lyrics, entered_top_30_in, name = "n_songs"), by = "entered_top_30_in") |>
  mutate(pct = n / n_songs) |>
  group_by(entered_top_30_in) |>
  slice_max(pct, n = 10, with_ties = FALSE) |>
  mutate(rank = row_number()) |>
  ungroup() |> 
  group_by(word) |>
  mutate(
    x_max = max(entered_top_30_in),
    y_max = rank[which.max(entered_top_30_in)]
  ) |> 
  ungroup() |> 
  mutate(
    col = case_when(
      y_max == 1 & x_max == max(x_max) ~ pal[1],
      y_max == 2 & x_max == max(x_max) ~ pal[2],
      y_max == 3 & x_max == max(x_max) ~ pal[3],
      y_max == 4 & x_max == max(x_max) ~ pal[4],
      y_max == 5 & x_max == max(x_max) ~ pal[5],
      TRUE ~ "gray70"
    )
  )

# geom_bump ignores the `group` aesthetic and lags by the other aes columns
# instead (ggbump::StatBump$setup_data). Patch it to group by `group`.
StatBump <- ggbump::StatBump
StatBump$setup_data <- function(data, params) {
  data |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::arrange(x) |>
    dplyr::group_by(group) |>
    dplyr::mutate(x_lag = dplyr::lag(x), y_lag = dplyr::lag(y)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.row) |>
    dplyr::select(-".row") |>
    as.data.frame()
}

f1 <- "Familjen Grotesk"
f2 <- "Montagu Slab 144pt"

ggplot(top_words, aes(x = entered_top_30_in, y = rank, group = word, color = col, size = pct)) +
  geom_bump(alpha = 0.8) +
  geom_point() +
  shadowtext::geom_shadowtext(aes(label = word, x = x_max, y = y_max), hjust = 0, nudge_x = 0.08, family = f2, size = 4.5, bg.color = "white") +
  scale_y_reverse(breaks = 10:1) +
  scale_x_continuous(breaks = 2013:2019) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = "Girl, time, love",
    subtitle = str_wrap("The ten words that turn up in the most country songs each year, across the 484 singles that reached the Top 30 of Billboard's Country Airplay chart. Words are ranked by the share of that year's songs using them at least once. Common words are removed. Words are counted as they appear, not grouped by meaning.", 134),
    caption = "Source: Grady Smith · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(margin = margin(t = 3, b = 15)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(10, 30, 10, 20)
  )
