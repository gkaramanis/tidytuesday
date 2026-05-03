library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# agreements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/agreements.csv')
# quantity_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/quantity_codes.csv')
tariff_agricultural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/tariff_agricultural.csv')
# tariff_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/tariff_codes.csv')


# Chapter names abbreviated from WCO HS Nomenclature 2022
# https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs-nomenclature-2022-edition/hs-nomenclature-2022-edition.aspx
chapter_names <- c(
  "01" = "Live animals", "02" = "Meat", "03" = "Fish & seafood", "04" = "Dairy, eggs & honey",
  "05" = "Animal products", "06" = "Live plants", "07" = "Vegetables",
  "08" = "Fruit & nuts", "09" = "Coffee, tea & spices", "10" = "Cereals",
  "11" = "Milling products", "12" = "Oil seeds", "13" = "Lac & gums",
  "14" = "Vegetable plaiting", "15" = "Fats & oils", "16" = "Meat & fish preparations",
  "17" = "Sugars", "18" = "Cocoa", "19" = "Cereal preparations",
  "20" = "Vegetable & fruit preparations", "21" = "Misc. edible preparations",
  "22" = "Beverages & spirits", "23" = "Food industry residues", "24" = "Tobacco"
)

plot_data <- tariff_agricultural |>
  filter(agreement == "mfn") |>
  mutate(chapter = str_sub(hts8, 1, 2)) |>
  group_by(chapter) |>
  mutate(has_quota = any(rate_type_code %in% c("K", "X"))) |>
  ungroup() |>
  filter(!rate_type_code %in% c("K", "X", "9")) |>
  group_by(hts8) |>
  slice_max(begin_effective_date, n = 1, with_ties = FALSE) |>
  group_by(chapter, has_quota) |>
  summarise(max_adval = max(ad_val_rate, na.rm = TRUE), .groups = "drop") |>
  mutate(
    chapter_name = chapter_names[chapter],
    type = factor(
      case_when(has_quota ~ "quota", max_adval > 0 ~ "taxed", TRUE ~ "free"),
      levels = c("free", "taxed", "quota")
    )
  ) |>
  arrange(max_adval) |>
  mutate(chapter_name = fct_inorder(chapter_name))

f1 <- "Karst"
f2 <- "Bricolage Grotesque 96pt Condensed"

ggplot(plot_data, aes(x = 0, xend = max_adval, y = chapter_name, color = type)) +
  geom_segment(linewidth = 6) +
  annotate("text", x = 0.25, y = "Sugars", label = "Sugar has a modest tax rate, but once\nthe annual limit is hit, no more gets in", size = 4.5, hjust = 0, family = f1, fontface = "bold", color = "#D4580A", lineheight = 0.95) +
  scale_x_continuous(breaks = c(0.25, 0.5, 1, 2, 3, 4), labels = scales::label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.18))) +
  scale_color_manual(values = c("taxed" = "#8BA7B0", "quota" = "#D4580A")) +
  coord_cartesian(clip = "off") +
  labs(
    title = toupper("The Sugar Wall"),
    subtitle = str_wrap("Highest import tax rate on agricultural goods entering the United States, by product category, as a share of the product's value. Tobacco (350%), raw peanuts (164%), and peanut butter (132%) carry the highest rates, reflecting decades of domestic farm protection. Sugar sits at 12% and is the only category with a hard annual import cap. Once the quota fills, trade stops.", 110),
    x = "Highest import tax in the category (% of value)",
    caption = "Source: USITC Tariff Database · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, ink = "grey20") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = f2, size = 16),
    axis.text.y = element_text(family = f2, size = 20),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, color = "#D4580A", face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 12)),
    plot.caption.position = "plot",
    plot.caption = element_text(margin = margin(t = 10), hjust = 0),
    plot.margin = margin(10, 20, 10, 20)
  )

record_polaroid()
