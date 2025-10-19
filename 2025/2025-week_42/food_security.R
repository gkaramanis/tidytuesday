library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

food_security <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-14/food_security.csv')

gaps <- food_security |> 
  filter(
    Item == "Prevalence of severe food insecurity in the female adult population (percent) (3-year average)" | Item == "Prevalence of severe food insecurity in the male adult population (percent) (3-year average)") |>
  select(Area, Year_Start, Item, Value) |>
  pivot_wider(names_from = Item, values_from = Value) |>
  select(area = 1, year = 2, male = 3, female = 4) |> 
  group_by(area) |>
  filter(year == max(year, na.rm = TRUE)) |> 
  mutate(diff = female - male) |> 
  ungroup() |> 
  filter(!is.na(diff)) |> 
  # Remove areas that are not countries
  filter(row_number() < match("World", area)) |> 
  mutate(area = fct_reorder(area, diff)) |> 
  arrange(area) |> 
  mutate(y = row_number())

n_female_higher <- gaps |> filter(diff > 0) |> nrow()
n_male_higher <- gaps |> filter(diff < 0) |> nrow()

bg_bars <- data.frame(x = seq(-8, 10, 2) + 1, y = 10, area = NA)

pal <- MetBrewer::met.brewer("Cassatt2", direction = -1)[3:8]
f1 <- "Outfit"

ggplot(gaps, aes(x = diff, y = y, label = area)) +
  geom_tile(data = bg_bars, aes(x, y, fill = x), height = Inf, width = 1.95) +
  geom_vline(xintercept = 0, color = "white", linewidth = 2) +
  geom_point(color = "black", size = 1, alpha = 0.9) +
  geom_rug(data = gaps |> filter(diff > 0), sides = "t", outside = TRUE, aes(color = diff > 0), position = "jitter", alpha = 0.8) +
  geom_rug(data = gaps |> filter(diff < 0), sides = "t", outside = TRUE, aes(color = diff > 0), position = "jitter", alpha = 0.8) +
  ggrepel::geom_text_repel(data = gaps |> filter(diff > 4), point.padding = 0.5, nudge_y = -0.5, family = f1, direction = "y", min.segment.length = 0.05, size = 4.5, segment.size = 0.3) +
  ggrepel::geom_text_repel(data = gaps |> filter(diff < -2), point.padding = 0.5, nudge_y = -0.5, family = f1, size = 4.5) +
  scale_x_continuous(breaks = seq(-8, 10, 2), expand = 0) +
  scale_color_manual(values = c("TRUE" = pal[6], "FALSE" = pal[2])) +
    scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0), na.value = "gray99") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Women face higher rates of severe food insecurity in most countries",
    subtitle = str_wrap(paste0(
      "In most countries, women are more likely to experience severe food insecurity. According to the latest FAO data, the prevalence is higher for women in ", n_female_higher, " countries, such as Pakistan, Afghanistan, and Peru, where limited land rights, lower income, and social restrictions make it harder for women to access food, especially during crises. In ", n_male_higher, " countries, including Jamaica, Libya, and Djibouti, men report higher rates—often in contexts where men are more likely to migrate for work, face job loss, or lack family support networks, which can reduce their access to regular meals."), width = 118),
    x = "Difference in prevalence of severe food insecurity (3-year average, 2022)",
    y = "Countries ordered by difference",
    caption = "Data: FAO · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(color = "gray20", size = 11, margin = margin(t = 5)),
    axis.title.x = element_text(color = "gray20", size = 14, margin = margin(t = 15)),
    axis.title.y = element_text(color = "gray20", size = 14, angle = 90, margin = margin(r = 5)),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(size = 24, face = "bold", color = "gray20", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 25)),
    plot.caption = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(t = 10))
  )

record_polaroid()
