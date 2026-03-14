library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

absolute_judgements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv')

respondent_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/respondent_metadata.csv')

jdg_highly <- absolute_judgements |> 
  filter(str_detect(term, "Highly")) |> 
  left_join(respondent_metadata) |> 
  filter(!is.na(english_background)) |>
  add_count(english_background) |>
  # Select top 5 countries by count
  filter(dense_rank(desc(n)) <= 5) |> 
  mutate(term_median = median(probability, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    english_background = fct_relevel(english_background, c(
      "English is my first language",
      "English is not my first language but I am fluent",
      "English is not my first language and I am not fluent"
      ))
    ) |> 
  count(english_background, term, probability, name = "n_bin") |>
  group_by(english_background) |>
  mutate(alpha_country = n_bin / sum(n_bin)) |>
  ungroup()

f1 <- "Karst ExtraBold"
f2 <- "Porpora"

ggplot(jdg_highly) +
  geom_col(aes( x = probability, y = n_bin, fill = term, alpha = alpha_country), position = "fill") +
  scale_fill_manual(values = c("#00965E", "#E07400"), guide = guide_legend(reverse = TRUE)) +
  scale_alpha(range = c(0.25, 1), guide = FALSE) +
  coord_cartesian(expand = FALSE) +
  facet_wrap(vars(english_background), ncol = 1) +
  labs(
    title = "Highly unlikely to mean the same thing",
    subtitle = "Adam Kucharski's CAPphrase survey explored how people interpret probability phrases. Based on over 9,000 responses for the phrases 'Highly likely' and 'Highly unlikely', fluent English speakers show wide disagreement on what these phrases mean. Non-fluent speakers take the words at face value and largely agree with each other, though the non-fluent group is much smaller (80 responses vs. over 1,400 fluent and 7,900 native speakers). Each bar shows a probability estimate from 0 to 100%, color intensity represents the proportion of responses within each group.",
    caption = "Source: Adam Kucharski · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12, family = f2),
    legend.key.width = unit(0.5, "line"),
    legend.key.spacing.x = unit(3, "line"),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(margin = margin(10, 0, 0, 0)),
    strip.text = element_text(size = 12, margin = margin(15, 0, 10, 0)),
    plot.title = element_text(size = 18),
    plot.subtitle = marquee::element_marquee(family = f2, width = 0.99, margin = margin(3, 0, 15, 0), lineheight = 1),
    plot.caption = element_text(size = 10, family = f2, margin = margin(15, 0, 0, 0)),
    plot.margin = margin(10, 15, 10, 15)
  )
  
record_polaroid()
