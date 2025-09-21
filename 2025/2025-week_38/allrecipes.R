library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 9, units = "in", dpi = 320)

# all_recipes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/all_recipes.csv')

cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv')

pasta <- c("pasta|spaghetti|fettuccine|linguine|macaroni|penne|rotini|ziti|cavatappi|bucatini|gemelli|tagliatelle|pappardelle|farfalle|orecchiette|conchiglie|tortellini|ravioli|lasagna|manicotti|cannelloni")

pasta_recipes <- cuisines |> 
  select(country, name, ingredients, calories, carbs, fat, protein) |>
  pivot_longer(cols = calories:protein, names_to = "nutrient", values_to = "value") |> 
  group_by(country, nutrient) |> 
  mutate(
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    norm_value = (value - min_value) / (max_value - min_value)
  ) |> 
  ungroup() |> 
  filter(nutrient == "carbs" | nutrient == "fat") |>
  select(-min_value, -max_value, -value) |>
  pivot_wider(names_from = nutrient, values_from = norm_value) |> 
  filter(str_detect(tolower(ingredients), pasta) | str_detect(tolower(name), pasta)) |> 
  filter(!is.na(carbs), !is.na(fat)) |>
  mutate(
    name = str_remove(name, paste0("( - )*", country, " (Style)*")),
    name = str_remove(name, "Cajun *"),
    label = case_when(
      carbs >= 0.5 | fat >= 0.5 | carbs == min(carbs) | fat == min(fat) ~ paste0("**", name, "**  \n", country),
      TRUE ~ country
    )
  )

annot <- tribble(
  ~carbs, ~fat, ~label, ~hjust, ~vjust,
  1, 0.53, "High carb ⮞", 1, 0,
  0, 0.53, "⮜ Low carb", 0, 0,
  0.48, 1, "High fat ⮝", 1, 1,
  0.52, 0, "⮟ Low fat", 0, 0
)

f1 <- "Fira Sans Condensed"
f2 <- "Outfit"

pal <- c(MetBrewer::met.brewer("Signac", direction = -1), MetBrewer::met.brewer("Renoir"))

ggplot(pasta_recipes) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.4, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.4, linewidth = 0.3) +
  geom_text(data = annot, aes(x = carbs, y = fat, label = label, hjust = hjust, vjust = vjust), size = 3.5, family = f2, alpha = 0.2, fontface = "bold") +
  geom_point(aes(x = carbs, y = fat, color = country)) +
  # Uses a fork of ggrepel with geom_marquee_repel()
  # devtools::install_github("teunbrand/ggrepel", ref = "marquee_repel")
  ggrepel::geom_marquee_repel(aes(x = carbs, y = fat, label = label, color = country), size = 3.2, family = f1, hjust = 0, lineheight = 0.95, seed = 777, segment.size = 0.2, box.padding = 0.2, point.padding = 0.8) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_color_manual(values = pal) +
  labs(
    title = "The pasta spectrum of carbs and fat",
    subtitle = str_wrap(paste0("How ", nrow(pasta_recipes), " pasta dishes from around the world compare in their carb and fat content per serving. Values were normalized across the 2 200 recipes of different dish types in the dataset."), 100),
    caption = "Data: Allrecipes via {tastyR} · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(t = 5, b = 20)),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 15))
  )

record_polaroid()

