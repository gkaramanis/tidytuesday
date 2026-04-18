library(tidyverse)
library(sf)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# beaufort_scale <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv')
# birds <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv')
# sea_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv')
ships <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv')

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

observers <- ships |> 
  add_count(observer, name = "on") |> 
  group_by(observer) |> 
  mutate(
    on = n(),
    years_range = paste(unique(range(year(date))), collapse = "–")
  ) |> 
  ungroup() |> 
  count(longitude, latitude, on, years_range, observer) |>
  mutate(
    observer_label = paste0("**", observer, "**  \n", scales::number(on), " obs. (", years_range, ")"),
    observer_label = fct_reorder(observer_label, -on)
    )

f1 <- "Karst"

design <- "
  ####AB
  CDEFGH
  IJKLMN
  OPQRST
  UVWXYZ
  "

ggplot(observers, aes(longitude, latitude)) +
  geom_sf(data = land, fill = "#f5eedf", color = "#ede0cc", linewidth = 0.1, inherit.aes = FALSE) +
  ggpointdensity::geom_pointdensity(aes(size = n), alpha = 0.5) +
  scale_size_area(max_size = 1) +
  MetBrewer::scale_color_met_c("Johnson", direction = -1) +
  coord_sf(xlim = c(65, 180), ylim = c(-75, -20)) +
  ggh4x::facet_manual(vars(observer_label), design = design) +
  labs(
    title = "The captain's logbook",
    subtitle = str_wrap("Captain John Arthur Francis Jenkins (1928–1989) recorded the vast majority of at-sea seabird sightings now held by Te Papa Tongarewa. Each map shows observations in the Southern Ocean by individual ship observers 1969–1990.", 80),
    caption = "Source: Museum of New Zealand Te Papa Tongarewa · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#d0dde3", color = NA),
    strip.text = marquee::element_marquee(style = modify_style(classic_style(), "body", align = "center"), lineheight = 1, margin = margin(t = 10, b = 3)),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 18, face = "bold", margin = margin(t = 20, b = -70, l = 10)),
    plot.subtitle = element_text(margin = margin(t = 75, b = -75, l = 10), lineheight = 1),
    plot.caption = element_text(hjust = 0, margin = margin(t = -460, b = 460, l = 10)),
    plot.margin = margin(10, 10, 10, 10)
    ) 

record_polaroid()
