library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

english_monarchs_marriages_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

marriages <- english_monarchs_marriages_df %>% 
  mutate(
    year_m = parse_number(year_of_marriage),
    monarch_i = as.numeric(factor(king_name)),
    monarch_a = parse_number(king_age),
    consort_a = parse_number(consort_age),
    monarch_b = year_m - monarch_a,
    consort_b = year_m - consort_a,
    nudge = if_else(monarch_a > consort_a, 1, -1)
  ) %>% 
  group_by(king_name) %>% 
  mutate(i = row_number()) %>% 
  ungroup() %>% 
  filter(str_detect(king_name, "Henry")) %>% 
  mutate(
    king_name = fct_reorder(king_name, -monarch_b),
    id = as.numeric(factor(king_name)),
    y = id * 2,
    age_diff = monarch_a - consort_a,
    nudge = nudge * 0.5
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

col1 <- "#e89c7b"
col2 <- "purple4"

# ggplot(marriages, aes(y = y)) +
#   # Monarch name
#   geom_text(data = . %>% filter(!id %in% c(2, 6:8)), aes(x = monarch_b - 8, label = king_name), hjust = 1, family = f2, alpha = 0.5, size = 6, stat = "unique", color = "grey30", fontface = "bold") +
#   geom_text(data = . %>% filter(id %in% c(2, 6:8)), aes(x = year_m + 8, label = king_name), hjust = 0, family = f2, alpha = 0.5, size = 6, stat = "unique", color = "grey30", fontface = "bold") +
#   # Monarch age
#   shadowtext::geom_shadowtext(aes(x = year_m, label = monarch_a, y = y + nudge * 1.5), size = 5, check_overlap = TRUE, family = f1b, color = col1, bg.color = "white", fontface = "bold") +
#   # Consort age diff
#   geom_segment(aes(x = year_m, xend = year_m, yend = y - age_diff / 5, color = age_diff > 0)) +
#   # Consort age
#   shadowtext::geom_shadowtext(aes(x = year_m, y = y - age_diff / 5 - nudge, label = consort_a), size = 3.5, family = f1b, color = col2, bg.color = "white", fontface = "bold") +
#   # Monarch line
#   geom_segment(aes(x = monarch_b, xend = year_m), size = 2, lineend = "square", color = "grey40") +
#   # Scales, theme, etc
#   scale_color_manual(values = c(col2, col1)) +
#   coord_cartesian(clip = "off") +
#   theme_minimal(base_family = f2) +
#   labs(
#     title = "Mind the age gap: the Henrys' marriages through the centuries",
#     subtitle = "A timeline of the marital ages and age gaps of the English kings named Henry, from the 12th to the 16th century.",
#     caption = "Source: Ian Visits · Graphic: Georgios Karamanis",
#     x = "Year",
#     y = "Age"
#   ) +
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(fill = "grey99", color = NA),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(size = 14),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(linewidth = 0.3),
#     panel.grid.minor.y = element_line(linewidth = 0.15),
#     plot.margin = margin(10, 10, 10, 50),
#     plot.title.position = "plot",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(family = f1)
#   )


# ggplot(marriages) +
#   # Connect
#   geom_segment(aes(x = year_m, y = monarch_a - nudge, xend = year_m, yend = consort_a), linewidth = 0.2, color = col2) +
#   # Monarchs
#   geom_segment(aes(x = monarch_b, xend = year_m, y = 0, yend = monarch_a), size = 1.5, color = col1, lineend = "round") +
#   geom_text(aes(x = monarch_b - 8, y = 0, label = king_name, color = king_name), angle = 77, hjust = 0, fontface = "bold", alpha = 0.3, stat = "unique") +
#   shadowtext::geom_shadowtext(aes(x = year_m - 5, y = monarch_a, label = monarch_a), hjust = 1, size = 2.5, color = col1, bg.color = "white", fontface = "bold") +
#   # Marriages
#   geom_point(aes(x = year_m, y = monarch_a), fill = "white", color = col1, stroke = 1, shape = 21, size = 2) +
#   # Consorts
#   geom_point(aes(x = year_m, y = consort_a, color = king_name), size = 2) +
#   shadowtext::geom_shadowtext(aes(x = year_m - 5, y = consort_a, label = paste0(consort_name, ", ", consort_a), color = king_name), hjust = 1, size = 2.5, family = f1b, lineheight = 0.8, direction = "x", bg.color = "white", fontface = "bold") +
#   # Scales, coord, etc
#   scale_y_continuous(breaks = seq(0, 60, 10), minor_breaks = 0:60) +
#   MetBrewer::scale_color_met_d("Redon", direction = -1) +
#   coord_cartesian(clip = "off") +
#   theme_minimal(base_family = f2) +
#   labs(
#     title = "Title",
#     subtitle = "Subtitle",
#     caption = "Source: Ian Visits · Graphic: Georgios Karamanis",
#     x = "Year",
#     y = "Age"
#   ) +
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(fill = "grey99", color = NA),
#     axis.title.x = element_text(margin = margin(10, 0, 0, 0), color = "grey30"),
#     axis.title.y = element_text(margin = margin(0, 10, 0, 0), color = "grey30"),
#     axis.text.x = element_text(size = 12),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_line(linewidth = 0.3),
#     panel.grid.minor.y = element_line(linewidth = 0.15),
#     plot.margin = margin(10, 20, 10, 10)
#   )

ggplot(marriages) +
  geom_col(aes(x = age_diff, y = king_name, fill = king_name), position = position_dodge2()) +
  geom_vline(xintercept = 0, color = "grey99", size = 7) +
  # Ages
  geom_text(aes(x = 0, y = king_name, label = monarch_a, color = king_name), position = position_dodge2(width = 0.9), family = f1b, size = 3) +
  geom_text(aes(x = age_diff - ((age_diff < 0) - 0.5) * 1.5, y = king_name, label = consort_a), position = position_dodge2(width = 0.9, reverse = TRUE), family = f1b, size = 3) +
  # Names
  geom_text(data = . %>% filter(king_name != "Henry II"), aes(x = -2, y = king_name, label = king_name, color = king_name), hjust = 1, family = f2, size = 6, stat = "unique", fontface = "bold") +
  geom_text(data = . %>% filter(king_name == "Henry II"), aes(x = 2, y = king_name, label = king_name, color = king_name), hjust = 0, family = f2, size = 6, stat = "unique", fontface = "bold") +
  MetBrewer::scale_color_met_d("Redon") +
  MetBrewer::scale_fill_met_d("Redon") +
  scale_x_continuous(breaks = seq(-10, 30, 10), labels = c("10 years\nyounger", "", "10 years\nolder", "20 years\nolder", "30 years\nolder")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Mind the age gap: the Henrys' marriages through the centuries",
    subtitle = str_wrap("Age gaps and marital ages of the English kings named Henry and their consorts, from the 12th to the 16th century.", 90),
    caption = "Source: Ian Visits · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 10, 10, 70),
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(family = f1, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(margin = margin(15, 0, 0, 0))
  )
