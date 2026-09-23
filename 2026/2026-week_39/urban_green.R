library(tidyverse)
library(countrycode)
library(ggrepel)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 9, units = "in", dpi = 320)

urban <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv')

urban_change <- urban |>
  # drop regional averages and missing years
  filter(!is.na(cityCode), !is.na(averageShareOfGreenAreaInCityUrbanAreaPct)) |>
  arrange(cityCode, year) |>
  # total change since 1990, last is the most recent year
  group_by(cityCode) |> 
  mutate(
    change = last(averageShareOfGreenAreaInCityUrbanAreaPct) - first(averageShareOfGreenAreaInCityUrbanAreaPct)
  ) |> 
  ungroup() |>
  # group subregions into continents
  mutate(
    region = case_when(
      str_detect(sdgSubRegion, "Africa") ~ "Africa",
      str_detect(sdgSubRegion, "Europe") ~ "Europe",
      sdgSubRegion == "Northern America" ~ "Northern America",
      sdgSubRegion %in% c("Caribbean", "Central America", "South America") ~ "Latin America & Caribbean",
      str_detect(sdgSubRegion, "Asia") ~ "Asia",
      str_detect(sdgSubRegion, "Australia|Melanesia|Micronesia|Polynesia") ~ "Oceania"
    )
  ) |>
  # keep top and bottom 3 cities per region
  group_by(region) |>
  mutate(
    top = dense_rank(desc(change)) <= 3,
    bottom = dense_rank(change) <= 3
  ) |> 
  ungroup() |> 
  filter(top | bottom) |>
  mutate(
    color = if_else(change >= 0, "Gained", "Lost"),
    country_code = countrycode(countryOrTerritoryName, "country.name", "iso3c", warn = FALSE),
    city_label = paste0(str_remove(cityName, "\\s*[,(].*"), " ", country_code)
  ) |>
  # change from 1990 in each year
  group_by(cityCode) |>
  mutate(
    change_since_1990 = averageShareOfGreenAreaInCityUrbanAreaPct - first(averageShareOfGreenAreaInCityUrbanAreaPct)
  ) |> 
  ungroup()

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Apfel Grotezk"

ggplot(urban_change, aes(x = year, y = change_since_1990, group = cityCode, color = color)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_point(data = filter(urban_change, year == max(year), .by = cityCode), size = 2) +
  geom_line(linewidth = 0.9) +
  ggrepel::geom_text_repel(data = filter(urban_change, year == max(year), .by = cityCode), aes(label = city_label), min.segment.length = Inf, xlim = c(2020, NA), family = f1, size = 4, bg.color = "white", hjust = 0) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2025), expand = expansion(mult = c(0.02, 0.1))) +
  scale_y_continuous(breaks = seq(-60, 20, 20), labels = scales::label_number(suffix = " pp", style_positive = "plus"), position = "right") +
  scale_color_manual(values = c("Gained" = "#0F5499", "Lost" = "#990F3D")) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(region)) +
  labs(
    title = "Cities lose green space far faster than they gain it",
    subtitle = "Change in the share of urban area covered by green space since 1990, for the three cities with the largest gains and losses in each region. Two in three of the 1,112 cities in the data lost green space. No city gained more than 18 percentage points, while three in West Africa lost more than 60.",
    caption = "Source: UN Habitat Urban Indicators · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 18) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_text(family = f2, face = "bold", hjust = 0, size = 17),
    plot.title = element_text(family = f2, face = "bold", size = 20, margin = margin(b = 7)),
    plot.subtitle = marquee::element_marquee(width = 0.99, size = 16, margin = margin(b = 15)),
    plot.caption = element_text(size = 13, margin = margin(t = 15), color = "grey40"),
    plot.margin = margin(20, 20, 20, 20)
  )
