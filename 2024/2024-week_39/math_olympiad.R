library(tidyverse)
library(camcorder)
library(ggrepel)
library(ggtext) 

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/country_results_df.csv')

chn_usa <- country_results_df %>%
  filter(country == "People's Republic of China" | country == "United States of America") %>% 
  select(year, country, awards_gold) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = country, values_from = awards_gold) %>% 
  mutate(across(2:3, function(x) replace_na(x, 0))) %>% 
  reframe(year, gold_diff = `People's Republic of China` - `United States of America`) %>% 
  mutate(
    bar_end = cumsum(gold_diff),
    bar_start = lag(bar_end, default = 0)
    )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"


# Totals: US 151, China 185
# First year: US 1974, China 1985

annotations <- tibble(
  year = c(1974, 1980, 1985, 1991, 2010, 2024),
  y_text = c(12, 3, 15, 8, 10, 20),
  y_point = c(0.5, 0.5, -14.5, 0.5, 24.5, 33.5),
  label = c(
    "USA joins IMO\nin 1974",
    "No IMO held\nin 1980",
    "China joins IMO in 1985,\n11 years after USA",
    "China catches up to USA\nin just its 7th participation",
    "China wins more gold awards\nfor 12 consecutive years",
    "Total gold awards in 2024\nChina 185, USA 151"
  )
)

ggplot(chn_usa) +
  # Zero line
  geom_hline(yintercept = 0, linewidth = 0.1) +
  # Text annotations
  geom_text(data = annotations, aes(x = year, y = y_text, label = label, vjust = ifelse(y_text > y_point, -0.2, 1.2), hjust = ifelse(year == 2024, 0.8, 0.5)), family = f1, lineheight = 1.1) +
  # Annotation segments
  geom_segment(data = annotations, aes(x = year, xend = year, y = y_text, yend = y_point), color = "#D4AF37", size = 0.2) +
  # Bars
  geom_rect(aes(xmin = year - 0.4, xmax = year + 0.4, ymin = bar_end, ymax = bar_start, fill = gold_diff < 0), color = "black", linewidth = 0.3) +
  # Labels
  geom_text(data = . %>% filter(gold_diff > 0), aes(x = year, y = bar_end + 0.5, label = gold_diff, color = gold_diff < 0), vjust = 0, family = f1b, fontface = "bold") +
  geom_text(data = . %>% filter(gold_diff < 0), aes(x = year, y = bar_end - 0.5, label = abs(gold_diff), color = gold_diff < 0), vjust = 1, family = f1b, fontface = "bold") +
  # Scales, labs, theme
  scale_x_continuous(breaks = seq(1974, 2024, 10)) +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c("#DE2910", "#3C3B6E")) +
  scale_color_manual(values = c("#DE2910", "#3C3B6E")) +
  labs(
    title = "China's golden rise in Mathematical Olympiads",
    subtitle = "Cumulative difference in gold awards between China and USA at the International Mathematical Olympiad (IMO). Each bar represents a single year, with upward <span style='color:#DE2910;'>**red bars**</span> showing China winning more golds, and downward <span style='color:#3C3B6E;'>**blue bars**</span> showing USA ahead. Chart begins with USA's 1974 debut; China joined 11 years later in 1985. Despite this late start, China overcame an initial 17-award deficit to lead by 34 in 2024, a remarkable 51-award swing.",
    caption = "Source: International Mathematical Olympiad · Graphic: Georgios Karamanis"
    ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(color = "grey60"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(family = f2, size = 23, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 12, lineheight = 1.3, margin = margin(5, 0, 10, 0), width = 0.92, hjust = 0),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(15, 10, 10, 10)
  )

