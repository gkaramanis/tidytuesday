library(tidyverse)
library(rvest)
library(legendry)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 7, units = "in", dpi = 320)

# euroleague_basketball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv')

url <- "https://en.wikipedia.org/wiki/FIBA_European_Champions_Cup_and_EuroLeague_records_and_statistics"

euroleague_wiki <- read_html(url) |> 
  html_elements(".wikitable.sortable") |>
  html_table() %>% 
  .[[1]]

el_titles <- euroleague_wiki |>
  arrange(-Champions, -`Runner-up`) |>
  mutate(
    Rank = row_number(),
    Champions = as.integer(Champions),
    `Runner-up` = as.integer(`Runner-up`)
  ) |> 
  replace_na(list(Titles = 0, `Runner-up` = 0)) |>
  mutate(pct = Champions / (Champions + `Runner-up`) * 100) |> 
  fill(Rank, .direction = "down")

el_years <- el_titles |> 
  filter(Rank <= 10) |> 
  separate_longer_delim(`Years won`, delim = ", ") |> 
  mutate(
    `Years won` = parse_number(`Years won`),
    `Years won` = `Years won`
  ) |> 
  arrange(Club, `Years won`) |> 
  group_by(Club) |>
  mutate(consecutive_titles = row_number()) |> 
  ungroup() |> 
  separate_longer_delim(`Years runner-up`, delim = ", ") |> 
  mutate(
    height = consecutive_titles / max(Champions),
    club_label = paste0("**", Club, "**<br>", Champions, ifelse(Rank == 1, " titles", ""), " · ", `Runner-up`, ifelse(Rank == 1, " runner-up", "")),
    club_label = fct_reorder(club_label, Rank)
  ) |> 
  separate_longer_delim(`Years runner-up`, delim = ", ") |> 
  mutate(
    `Years runner-up` = parse_number(`Years runner-up`),
    `Years runner-up` = `Years runner-up`
  )

el_names <- tribble(
  ~name, ~start, ~end,
  "FIBA European Champions Cup", 1958, 1990.5,
  "FIBA European\nLeague", 1991, 1995.5,
  "FIBA\nEuroLeague", 1996, 1999.5,
  "EuroLeague (ECA)", 2000, Inf
)

el_key <- key_range_manual(
  start = el_names$start - 0.5,
  end = el_names$end - 0.5,
  name = el_names$name,
  level = 1
)

f1 <- "Roboto Condensed"

ggplot(el_years) +
  geom_segment(data = el_names, aes(x = start - 0.5, xend = start -0.5, y = -0.5, yend = 0.5), color = "white", linewidth = 1) +
  geom_tile(aes(x = `Years runner-up`, y = 0), height = 1, width = 1, color = "white", fill = "grey88", stat = "unique") +
  geom_tile(aes(x = `Years won`, y = 0, fill = height), width = 1, color = "white", stat = "unique") +
  scale_x_continuous(breaks = seq(1960, 2024, by = 5), expand = 0, sec.axis = dup_axis(guide = guide_axis_nested(regular_key = "auto", key = el_key, position = "top"), breaks = c(el_names$start, max(el_years$`Years won`)))) +
  # scale_x_continuous(breaks = seq(1960, 2020, by = 10), expand = 0) +
  scale_y_reverse(expand = 0) +
  MetBrewer::scale_fill_met_c("Tam") +
  facet_wrap(vars(club_label), ncol = 1, strip.position = "left") +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f1) +
  labs(
    title = "From FIBA European Champions Cup to EuroLeague",
    subtitle = str_wrap("The top 10 clubs by championships across 67 years (1958–2024) and four formats: FIBA European Champions Cup (1958–1990), FIBA European League (1991–1995), FIBA EuroLeague (1996–1999), and EuroLeague (2000–present). Colored tiles mark wins, gray tiles mark finals losses. Darker shades show cumulative titles. Real Madrid leads with 11 titles and 10 runner-up finishes. CSKA Moscow has 8 titles across both its Soviet and Russian eras. Panathinaikos won 7 of its 8 finals appearances.", 166),
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_guide(
    bracket = element_line(color = "grey50", linewidth = 0.2)
    ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey95", color = NA),
    axis.text.x = element_text(size = 9, margin = margin(5, 0, 5, 0)),
    strip.text = element_markdown(hjust = 1, margin = margin(0, 5, 0, 0), size = 9, lineheight = 1.2),
    plot.title.position = "plot",
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0)),
    plot.margin = margin(15, 15, 15, 15)
  )

record_polaroid()

