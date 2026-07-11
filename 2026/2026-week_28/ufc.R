library(tidyverse)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

ufc_fights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-07/ufc_fights.csv')

weights <- c("Strawweight", "Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Light Heavyweight", "Heavyweight")

# gender-prefixed so levels stay unique across the two facets (needed for
# per-division axis labels); the prefix is stripped again for display
wc_levels <- paste(rep(c("Women's", "Men's"), each = length(weights)), weights)

divisions <- ufc_fights |>
  filter(!is.na(method)) |>
  mutate(
    weight = str_extract(weight_class, str_flatten(weights, "|")),
    gender = fct(if_else(str_detect(weight_class, "Women's"), "Women's", "Men's"), levels = c("Women's", "Men's")),
    wc = factor(paste(gender, weight), levels = wc_levels),
    how = case_when(
      str_detect(method, "KO|TKO") ~ "Knockout",
      str_detect(method, "Submission") ~ "Submission",
      str_detect(method, "Decision") ~ "Decision",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(wc), !is.na(how)) |>
  count(gender, wc, how) |>
  mutate(share = n / sum(n), .by = c(gender, wc)) |>
  mutate(
    x = if_else(how == "Decision", share, -share),
    how = fct_relevel(how, "Knockout", "Submission", "Decision")
  )

f1 <- "DIN Condensed"
f2 <- "Graphik"

pal <- c(Knockout = "#b1402f", Submission = "#4a85a7", Decision = "#c8c1ae")

# the Decision beige is fine as a fill but too pale as text
dec_dark <- colorspace::darken(pal["Decision"], 0.2)

axis_labels <- divisions |>
  pivot_wider(id_cols = wc, names_from = how, values_from = n, values_fill = 0) |>
  mutate(
    label = paste0(
      str_remove(wc, "^(Women's|Men's) "), "  \n",
      "{.", pal["Knockout"], " **", Knockout, "**} ",
      "{.", pal["Submission"], " **", Submission, "**} ",
      "{.", dec_dark, " **", Decision, "**}"
    )
  ) |>
  select(wc, label) |>
  deframe()

ggplot(divisions, aes(x, wc, fill = how)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, color = "grey99", linewidth = 0.8) +
  geom_text(aes(label = if_else(how == "Knockout", scales::percent(share, accuracy = 1), NA_character_)), fontface = "bold", position = position_stack(vjust = 0.5), color = "white") +
  scale_x_continuous(labels = ~scales::percent(abs(.x)), breaks = seq(-0.75, 0.75, 0.25), limits = c(-0.75, 0.75)) +
  scale_y_discrete(limits = rev, labels = axis_labels) +
  scale_fill_manual(values = pal) +
  ggforce::facet_col(vars(gender), strip.position = "left", scales = "free", space = "free") +
  labs(
    title = "Heavier hands, earlier nights",
    subtitle = paste0(
      "How every UFC fight since 1994 has ended, division by division, from lightest to heaviest. Bars grow to the left ",
      "when fights ended early, by {.", pal["Knockout"], " **knockout**} or {.", pal["Submission"], " **submission**}, ",
      "and to the right when they went to the judges. The white number is each division's knockout share; the small ",
      "numbers under each name count its {.", pal["Knockout"], " **knockouts**}, {.", pal["Submission"], " **submissions**} ",
      "and {.", dec_dark, " **decisions**}."
    ),
    caption = "Source: UFC Stats via the fightr R package · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_marquee(family = f1, size = 12, lineheight = 1.15, hjust = 1, style = modify_style(classic_style(), "body", align = "right")),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 15, family = f1, angle = 90),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    axis.text.x = element_text(size = 9, margin = margin(t = 3), color = "grey30"),
    plot.title.position = "plot",
    plot.title = element_text(family = f1, size = 24, face = "bold"),
    plot.subtitle = element_marquee(size = 12, margin = margin(b = 15), width = 1, lineheight = 1),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(15, 15, 10, 15)
  )

