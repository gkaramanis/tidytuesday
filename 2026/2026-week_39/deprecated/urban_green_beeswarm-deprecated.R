library(tidyverse)
library(ggbeeswarm)
library(ggiraph)
library(legendry)
library(htmltools)
library(htmlwidgets)
# library(camcorder)

# gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

urban <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv')

pal <- RColorBrewer::brewer.pal("Dark2", n = 6)

urban_latest <- urban |> 
  filter(!is.na(averageShareOfGreenAreaInCityUrbanAreaPct)) |> 
  filter(!is.na(greenAreaPerCapitaM2)) |>
  slice_max(year, by = cityCode, n = 1) |> 
  mutate(
     region = case_when(
      sdgRegion == "Global Average" ~ "Global Average",
      str_detect(sdgSubRegion, "Africa") ~ "Africa",
      str_detect(sdgSubRegion, "Europe") ~ "Europe",
      sdgSubRegion == "Northern America" ~ "Northern America",
      sdgSubRegion %in% c("Caribbean", "Central America", "South America") ~ "Latin America & Caribbean",
      str_detect(sdgSubRegion, "Asia") ~ "Asia",
      str_detect(sdgSubRegion, "Australia|Melanesia|Micronesia|Polynesia") ~ "Oceania"
    )
  ) |> 
  filter(!is.na(region)) |> 
  mutate(
    region_color = case_when(
      region == "Global Average" ~ "grey50",
      region == "Africa" ~ pal[1],
      region == "Europe" ~ pal[2],
      region == "Northern America" ~ pal[3],
      region == "Latin America & Caribbean" ~ pal[4],
      region == "Asia" ~ pal[5],
      region == "Oceania" ~ pal[6]
    ),
    tooltip_text = paste0(if_else(is.na(cityName), "", paste0("<b>", cityName, "</b>, ")), countryOrTerritoryName, "<br>Share of green area: ", round(averageShareOfGreenAreaInCityUrbanAreaPct, 1), "%<br>Green area per capita: ", round(greenAreaPerCapitaM2, 1), " m²")
  )

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Apfel Grotezk"

p <- ggplot(urban_latest, aes(x = region, y = averageShareOfGreenAreaInCityUrbanAreaPct, group = region)) +
  geom_point_interactive(aes(tooltip = tooltip_text, data_id = cityName, size = greenAreaPerCapitaM2, fill = region_color, color = after_scale(colorspace::darken(fill, 0.8))), alpha = 0.7, shape = 21, position = position_quasirandom(bandwidth = 0.8), hover_nearest = TRUE) +
  scale_fill_identity(guide = "none") +
  scale_size_area(max_size = 10, breaks = c(20, 40, 60), labels = c(20, "", 60)) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(
    title = "Urban green areas",
    subtitle = "Share of green area in urban areas and green area per capita in cities, by region",
    caption = "Source: UN Habitat Urban Indicators · Graphic: Georgios Karamanis"
  ) +
  guides(size = guide_circles()) +
  theme_minimal(base_family = f1, base_size = 20) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(family = f2, face = "bold")
  )


girafe(p, width_svg = 11, height_svg = 8, 
  options = list(
    opts_tooltip(use_fill = TRUE, css = "font-family:'Sofia Sans Extra Condensed'; font-size:20px; color:#fff; padding:3px 7px; border-radius:3px;")
    )
  ) |> 
  prependContent(
    htmltools::tags$style(htmltools::HTML("@import url('https://fonts.googleapis.com/css2?family=Sofia+Sans+Extra+Condensed:wght@400;700&display=swap');
    
    @font-face {
    font-family: 'Apfel Grotezk';
    src: url('https://karaman.is/fonts/ApfelGrotezk-Regular.woff2') format('woff2');
    }
    
    @font-face {
    font-family: 'Apfel Grotezk';
    font-weight: bold;
    src: url('https://karaman.is/fonts/ApfelGrotezk-Fett.woff2') format('woff2');
    }
"
)))


# ggiraph
# legendry for circles legend
# inspiration https://flowingdata.com/2026/05/07/divorce-and-occupation-2026/
# with some help from https://nrennie.rbind.io/blog/interactive-beeswarm-r/
