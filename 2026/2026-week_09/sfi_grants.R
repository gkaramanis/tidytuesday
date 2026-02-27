library(tidyverse)
library(tidytext)
library(ggfittext)
library(scales)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)

sfi_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-24/sfi_grants.csv')

sfi_totals <- sfi_grants |> 
  mutate(year_start = year(start_date)) |> 
  filter(year_start < 2025) |> 
  group_by(year_start) |> 
  summarise(total = sum(current_total_commitment)) |> 
  ungroup()

sfi_tokens <- sfi_grants |> 
  mutate(year_start = year(start_date)) |> 
  filter(year_start < 2025) |> 
  unnest_tokens(input = proposal_title, output = word, token = "words") |> 
  anti_join(get_stopwords()) |> 
  filter_out(word %in% year_start) |> 
  count(year_start, word) |>
  bind_tf_idf(term = word, document = year_start, n = n) |>
  mutate(word_n = paste0(word, "<span style='color:#FFD166'><sup>", n, "</sup></span>")) |>
  group_by(year_start) |>
  slice_max(tf_idf, n = 3) |> 
  arrange(-n) |> 
  summarize(tokens = paste(word_n, collapse = " ")) |> 
  ungroup()

sfi <- sfi_totals |> 
  left_join(sfi_tokens)

f1 <- "Fixel Display"
f2 <- "JetBrains Mono"

annot <- tribble(
  ~x,    ~y,          ~label,
  2006,  238e6,   "Equipment funding ↓",
  2012,  256e6,   "ERC & ISCA funding surge ↓",
  2018,  405e6,   "Peak year on record ↓",
  2020,  135e6,   "← COVID-19 response grants"
)

ggplot(sfi |> filter(year_start >= 2005), aes(year_start, total, label = tokens)) +
  geom_col(fill = "#4A525A") +
  geom_bar_text(color = "white", grow = TRUE, family = f2, place = "center", rich = TRUE, min.size = 3) +
  geom_text(data = annot, aes(x = x, y = y, label = label), hjust = 0, size = 3, family = f1, fontface = "bold", color = "#4A525A") +
  coord_flip(clip = "off") +
  scale_x_reverse(breaks = c(seq(2005, 2020, 5), 2024), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(labels = label_number(scale_cut = cut_long_scale()), expand = expansion(mult = 0), name = "Total committed funding (€)") +
  labs(
    title = "Science Foundation Ireland Grants 2005-2024",
    subtitle = "Each bar shows total grant funding committed per year. The words inside are the 3 most distinctive terms (more if tied) from proposal titles, ranked by TF-IDF (frequent in that year, rare in others). Superscript {.#E9C781 **numbers**} indicate how many proposals used that word.",
    caption = "Source: Ireland’s Open Data Portal · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#F7F9FC", color = NA),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = marquee::element_marquee(size = 10, color = "grey40", margin = margin(b = 20), width = 0.95, lineheight = 0.95),
    plot.margin = margin(10, 20, 10, 15)
  )
  
record_polaroid()
  