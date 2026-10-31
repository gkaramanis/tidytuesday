library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

ikea_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_places.csv')
ikea_seating <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_seating.csv')

poang_index <- ikea_seating |>
  filter(item_no == "39240787") |> 
  mutate(price_se_ratio = price_usd / price_usd[country == "se"]) |> 
  mutate(country = countrycode::countrycode(country, origin = "iso2c", destination = "country.name"),
    country_label = case_when(
      country == "United Arab Emirates" ~ "UAE",
      country == "United States" ~ "USA",
      TRUE ~ country
    ),
    country_label = fct_reorder(country_label, price_se_ratio)
  )
  
f2 <- "Fantasque Sans Mono"

poang_img <- data.frame(x = 12, y = 1.85, image = here::here("2026/2026-week_44/img/poang.png"))

ggplot(poang_index, aes(x = country_label, y = price_se_ratio)) +
  geom_col(aes(fill = country == "Sweden"), width = 0.85) +
  ggimage::geom_image(data = poang_img, aes(x, y, image = image), size = 0.35) +
  scale_fill_manual(values = c("#BEB8B0", "#801818"), guide = "none") +
  scale_y_continuous(limits = c(0, 2.3), minor_breaks = seq(0, 2.3, by = 0.1)) +
  coord_flip(expand = FALSE) +
  labs(
    title = "The POÄNG index",
    subtitle = "The same POÄNG armchair costs $65 in Romania and $181 in Finland, more than twice the $80 it costs in Sweden. Each bar shows the price in one of 38 markets relative to Sweden. Prices from ikea.com on 28 September 2026, in US dollars.",
    caption = "Source: ikea.com · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 14) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = marquee::element_marquee(width = 0.95, lineheight = 1, margin = margin(b = 20)),
    plot.caption = element_text(margin = margin(t = 20)),
    plot.margin = margin(20, 20, 20, 20)
  )
