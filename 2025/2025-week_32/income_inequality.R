library(tidyverse)
library(marquee)
library(ggforce)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 9, units = "in", dpi = 320)

income_inequality_processed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_processed.csv') 
income_inequality_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_raw.csv')

f1 <- "Outfit"

regions <- income_inequality_raw |> 
  filter(!is.na(owid_region)) |>
  distinct(Code, Entity, owid_region)

s_set <- style_set(base = base_style())
sm <- modify_style(s_set, "sm", size = 9, color = "grey55")

ii_latest <- income_inequality_processed |> 
  filter(!is.na(gini_mi_eq) & !is.na(gini_mi_eq)) |>
  group_by(Code) |> 
  filter(Year == max(Year)) |> 
  ungroup() |> 
  left_join(regions) |>
  group_by(owid_region) |> 
  mutate(
    ratio = gini_dhi_eq / gini_mi_eq,
    entity_year = marquee_glue("{Entity} {.sm {Year}}",),
    entity_year = fct_reorder(entity_year, gini_dhi_eq)
  ) |> 
  ungroup()

ggplot(ii_latest, aes(x = gini_mi_eq, xend = gini_dhi_eq, y = entity_year, yend = entity_year)) +
  geom_link(aes(size = after_stat(index), color = log(ratio)), n = 50) +
  facet_col(vars(owid_region), scales = "free_y", space = "free") +
  scale_size_continuous(range = c(0.35, 3)) +
  MetBrewer::scale_color_met_c("Hokusai2") +
  coord_cartesian(clip = "off") +
  labs(
    title = "How government policies reshape income inequality",
    subtitle = str_wrap("Each line shows the change from market income to disposable income inequality using Gini coefficients. European countries generally show stronger redistribution effects. South Africa has both the highest initial  and final inequality despite some redistribution. Belgium cuts inequality nearly in half, while Dominican Republic barely reduces it at all. Belgium and Greece start with similar inequality but Belgium achieves much deeper cuts. Latest available year shown for each country.", 105),
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_family = f1, base_size = 12) +
  theme(
    legend.position = "inside",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text.x = element_text(hjust = 1, face = "bold", margin = margin(0, 450, 0, 0), size = 10),
    axis.title = element_blank(),
    axis.text = element_marquee(style = sm),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
