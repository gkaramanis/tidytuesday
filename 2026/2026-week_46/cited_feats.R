library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 6, units = "in", dpi = 320)

fiction_citations <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/fiction_citations.csv")
fiction_rankings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/fiction_rankings.csv")

# Westeros character citations only; armaments have no rank
ticks <- fiction_citations |>
  filter(franchise_slug == "westeros", subject_kind == "entry") |>
  inner_join(
    select(fiction_rankings, franchise_slug, entry_id, name, rank),
    by = join_by(franchise_slug, subject_id == entry_id),
    relationship = "many-to-one"
  ) |>
  mutate(confidence = fct_relevel(confidence, "shown", "scaled", "stated")) |>
  arrange(is.na(rank), rank, name, confidence) |>
  # Characters with more stated than shown citations
  mutate(x = row_number(), hearsay = mean(confidence == "stated") > 0.5, .by = name) |>
  mutate(
    label = if_else(is.na(rank), str_glue("– {name}"), str_glue("#{rank} {name}")),
    label = fct_inorder(label),
    tick = str_c(if_else(hearsay, "hearsay", "seen"), "_", confidence)
  )

f2 <- "Founders Grotesk Text"

bg <- "grey96"
ink <- "grey15"

# Hue marks the character, brightness the citation
tick_cols <- c(hearsay_shown = "#6E3B26", hearsay_stated = "#DDBBA6", seen_shown = "#24414F", seen_stated = "#A7BFC9")

ggplot(ticks) +
  geom_tile(aes(x, 0, fill = tick), width = 0.75, height = 1) +
  # Invisible points that only feed the grey brightness legend and the "–" entry
  geom_point(data = distinct(ticks, confidence), aes(1, 0, colour = confidence), alpha = 0, key_glyph = "rect") +
  geom_point(data = slice_head(ticks, n = 1), aes(x, 0, shape = "Unranked"), alpha = 0) +
  scale_colour_manual(values = c(shown = "grey20", stated = "grey75"), labels = c(shown = "Shown on page or screen", stated = "Only stated"), name = "One tick per citation") +
  scale_shape_manual(values = "–", name = NULL) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(alpha = 1)),
    fill = "none",
    shape = guide_legend(order = 3, override.aes = list(size = 4, colour = ink, alpha = 1))
  ) +
  scale_fill_manual(values = tick_cols, breaks = c("hearsay_shown", "seen_shown"), labels = c("Mostly stated", "Mostly shown"), name = NULL) +
  scale_x_continuous(limits = c(0.5, max(ticks$x) + 0.5), expand = expansion(0)) +
  facet_wrap(vars(label), ncol = 5) +
  labs(title = "One in three Westeros characters rests mostly on hearsay", subtitle = "Cited Feats is a fan site that ranks fictional characters by strength. Every place rests on citations from the books and shows. Characters in {#6E3B26 **brown**} have more stated than shown feats. {#6E3B26 **Rhaegar**}, {#6E3B26 **Robert**} and {#6E3B26 **Arthur Dayne**} fought before the story begins. {#6E3B26 **The Targaryens of the Dance**} live mostly in an in-world history book. Characters in {#24414F **blue**}, like {#24414F **Jon**}, {#24414F **Brienne**} and {#24414F **Arya**}, fight in front of us.", caption = "Source: Cited Feats · Graphic: Georgios Karamanis") +
  theme_void(base_family = f2) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 11, colour = ink, margin = margin(0, 10, 0, 0)),
    legend.text = element_text(size = 10, colour = ink, margin = margin(0, 14, 0, 4)),
    legend.key.width = unit(0.5, "lines"),
    legend.key.height = unit(1, "lines"),
    legend.margin = margin(0, 0, 12, 0),
    plot.background = element_rect(fill = bg, colour = NA),
    plot.title = element_text(size = 20, face = "bold", colour = ink, margin = margin(0, 0, 6, 0)),
    plot.subtitle = marquee::element_marquee(width = 1, size = 11, colour = ink, lineheight = 1, margin = margin(0, 0, 24, 0)),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 0, margin = margin(14, 0, 0, 0)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.text = marquee::element_marquee(size = 9, family = f2, style = marquee::classic_style(weight = "bold"), colour = ink, hjust = 0, margin = margin(0, 0, 4, 0)),
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(1, "lines"),
    plot.margin = margin(20, 25, 15, 25)
  )
