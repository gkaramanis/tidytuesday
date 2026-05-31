library(tidyverse)
library(countrycode)
library(geofacet)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8.5, units = "in", dpi = 320)

energy_cleaned <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-26/energy_cleaned.csv')

biomass <- energy_cleaned |> 
  mutate(country_code = countrycode(country_name, origin = "country.name", destination = "iso3c")) |>
  select(country_name, country_code, yr, traditional_biomass_consumption_tfec_pct) |> 
  filter(!is.na(traditional_biomass_consumption_tfec_pct)) |>
  mutate(country_code = case_when(
    country_name == "Kosovo" ~ "XKX",
    country_name == "Netherlands Antilles" ~ "ANT",
    country_name == "World" ~ "World",
    country_name == "Europe" ~ "Europe",
    country_name == "Eastern Europe" ~ "East. Europe",
    country_name == "Caucasus and Central Asia" ~ "Caucasus & C. Asia",
    country_name == "Western Asia" ~ "West. Asia",
    country_name == "Southern Asia" ~ "South. Asia",
    country_name == "South Eastern Asia" ~ "S. East. Asia",
    country_name == "Oceania" ~ "Oceania",
    country_name == "Northern Africa" ~ "North. Africa",
    country_name == "Sub-Saharan Africa" ~ "Sub-Sah. Africa",
    country_name == "Latin America and Caribbean" ~ "Latin Am. & Carib.",
    country_name == "Nothern America" ~ "North. America",
    country_name == "High income" ~ "High income",
    country_name == "Upper middle income" ~ "Upper middle",
    country_name == "Lower middle income" ~ "Lower middle",
    country_name == "Low income" ~ "Low income",
    TRUE ~ country_code
  ))

new_rows <- tribble(
  ~name, ~code_alpha3, ~code_country, ~code_iso_3166_2, ~row, ~col,
  "Saint Pierre et Miquelon", "SPM", "666", "ISO 3166-2:PM", 1, 2,
  "Bermuda", "BMU", "060", "ISO 3166-2:BM", 2, 6,
  "Turks and Caicos Islands", "TCA", "796", "ISO 3166-2:TC", 3, 6,
  "Puerto Rico", "PRI", "630", "ISO 3166-2:PR", 3, 7,
  "British Virgin Islands", "VGB", "092", "ISO 3166-2:VG", 3, 8,
  "Cayman Islands", "CYM", "136", "ISO 3166-2:KY", 4, 3,
  "Montserrat", "MSR", "500", "ISO 3166-2:MS", 4, 8,
  "Guadeloupe", "GLP", "312", "ISO 3166-2:GP", 5, 8,
  "Martinique", "MTQ", "474", "ISO 3166-2:MQ", 7, 8,
  "Aruba", "ABW", "533", "ISO 3166-2:AW", 8, 5,
  "Netherlands Antilles", "ANT", "530", "ISO 3166-2:AN", 8, 6,
  "French Guyana", "GUF", "254", "ISO 3166-2:GF", 10, 7,
  "Falkland Islands", "FLK", "238", "ISO 3166-2:FK", 15, 7,
  "Gibraltar", "GIB", "292", "ISO 3166-2:GI", 7, 12,
  "Western Sahara", "ESH", "732", "ISO 3166-2:EH", 11, 11,
  "Reunion", "REU", "638", "ISO 3166-2:RE", 20, 18,
  "Macao SAR, China", "MAC", "446", "ISO 3166-2:MO", 8, 25,
  "Hong Kong SAR, China", "HKG", "344", "ISO 3166-2:HK", 8, 26,
  "New Caledonia", "NCL", "540", "ISO 3166-2:NC", 19, 25,
  "Cook Islands", "COK", "184", "ISO 3166-2:CK", 20, 27,
  "French Polynesia", "PYF", "258", "ISO 3166-2:PF", 20, 28
)

agg_grid <- tribble(
  ~name, ~code_alpha3, ~code_country, ~code_iso_3166_2, ~row, ~col,
  "World", "World", "WLD", "WLD", 23, 2,
  "Nothern America", "North. America", "NAR", "NAR", 23, 4,
  "Latin America and Caribbean", "Latin Am. & Carib.", "LAC", "LAC", 23, 6,
  "Europe", "Europe", "EUR", "EUR", 23, 8,
  "Northern Africa", "North. Africa", "NAF", "NAF", 23, 9,
  "Sub-Saharan Africa", "Sub-Sah. Africa", "SSA", "SSA", 23, 11,
  "Eastern Europe", "East. Europe", "EEU", "EEU", 23, 12,
  "Western Asia", "West. Asia", "WAS", "WAS", 23, 13,
  "Caucasus and Central Asia", "Caucasus & C. Asia", "CCA", "CCA", 23, 15,
  "Southern Asia", "South. Asia", "SAS", "SAS", 23, 17,
  "South Eastern Asia", "S. East. Asia", "SEA", "SEA", 23, 18,
  "Oceania", "Oceania", "OCE", "OCE", 23, 20,
  "High income", "High income", "HIC", "HIC", 23, 24,
  "Upper middle income", "Upper middle", "UMC", "UMC", 23, 25,
  "Lower middle income", "Lower middle", "LMC", "LMC", 23, 26,
  "Low income", "Low income", "LIC", "LIC", 23, 27
)

world_grid <- world_countries_grid1 |>
  as_tibble() |>
  filter(name != "Antarctica") |>
  bind_rows(new_rows) |>
  bind_rows(agg_grid)

grid_labels <- world_grid |>
  mutate(
    country_code = code_alpha3, 
    yr = mean(range(biomass$yr)),
    y = -0.73, 
    label = str_wrap(country_code, 9)
  )

f1 <- "Space Grotesk"

ggplot(biomass, aes(x = yr, y = 0, fill = traditional_biomass_consumption_tfec_pct)) +
  geom_tile() +
  geom_text(data = grid_labels, aes(x = yr, y = y, label = label), inherit.aes = FALSE, size = 2.5, vjust = 1, lineheight = 0.9, fontface = "bold") +
  scale_fill_viridis_b(na.value = NA, breaks = seq(0, 100, 10), labels = \(x) ifelse(x %% 20 == 0, x, "")) +
  coord_cartesian(ylim = c(-1, 0.5), clip = "off") +
  facet_geo(vars(country_code), grid = world_grid) +
  labs(
    title = "Renewable by necessity",
    subtitle = str_wrap("From 1990 to 2010, the world's poorest countries already ran on energy that counts as renewable. Traditional biomass, mostly firewood, charcoal and dung burned in open stoves, met over half of all energy use in low-income countries and more than 90% in Ethiopia, while rich economies had left it behind. But this was a sign of poverty, not a green choice. The fires degrade forests and fill homes with smoke that the WHO links to millions of deaths a year, mostly women and children.", 165),
    caption = "Source: Sustainable Energy for all (SE4ALL) · Graphic: Georgios Karamanis",
    fill = "Traditional biomass, % of final energy"
  ) +
  theme_void(base_family = f1, base_size = 10) +
  theme(
    plot.background = element_rect(fill = "#FAF7F1", color = NA),
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 9, face = "bold"),
    legend.key.height = unit(0.55, "lines"),
    legend.key.width = unit(2.2, "lines"),
    legend.margin = margin(0, 0, 10, 0),
    strip.text = element_blank(),
    panel.spacing.x = unit(0.5, "lines"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 20, b = -5))
  )

record_polaroid()