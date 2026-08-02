library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

# Python popgrid via reticulate. First call creates the virtualenv.
source(here::here("2026/2026-week_31/popgrid_py.R"))

# occurrences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/occurrences.csv')
tourism <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/tourism.csv')
# weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-28/weather.csv')

# Prepare GCCSA polygons for popgrid
gccsa_raw <- read_sf(here::here("2026/2026-week_31/data/GCCSA_2026_AUST_SHP_GDA2020/")) 

gccsa <- gccsa_raw |> 
   filter(!st_is_empty(geometry)) |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON") |>
  filter(as.numeric(st_area(geometry)) > 1e6) |>   # keep parts > 1 km²
  group_by(GCC_NAME26) |>
  summarise(.groups = "drop")

# Assign each trip to a GCCSA and calculate the share of trips by quarter
holiday_q <- tourism |> 
  filter(purpose == "Holiday") |>
  filter(!is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 7844) |> 
  st_join(gccsa, join = st_within) |> 
  filter(!is.na(GCC_NAME26)) |> 
  group_by(GCC_NAME26, quarter) |> 
  summarise(trips_total = sum(trips, na.rm = TRUE)) |> 
  ungroup() |> 
  st_drop_geometry() |> 
  group_by(quarter) |> 
  mutate(trips_share = 100 * trips_total / sum(trips_total, na.rm = TRUE)) |> 
  ungroup() 

quarter_cells <- function(q, n = 1200) {
  gccsa |>
    left_join(holiday_q, by = "GCC_NAME26") |>
    filter(quarter == q) |>
    # This call will install Python and the popgrid package if not already installed in the virtualenv
    popgrid_py(region_col = "GCC_NAME26", weight_col = "trips_share", n = n)
}

cells_q1 <- quarter_cells(1)
cells_q3 <- quarter_cells(3)

f1 <- "Iosevka Charon"

pal_state <- c(
  NSW = "#2a6f97", Vic = "#bb4430", Qld = "#e8a33d", SA  = "#8c5f8a",
  WA  = "#4f8f6b", NT  = "#c85d3c", Tas = "#45636f", ACT = "#9578b6"
)

state_of <- c(
  "Greater Sydney"    = "NSW", "Rest of NSW" = "NSW",
  "Greater Melbourne" = "Vic", "Rest of Vic" = "Vic",
  "Greater Brisbane"  = "Qld", "Rest of Qld" = "Qld",
  "Greater Adelaide"  = "SA",  "Rest of SA"  = "SA",
  "Greater Perth"     = "WA",  "Rest of WA"  = "WA",
  "Greater Hobart"    = "Tas", "Rest of Tas" = "Tas",
  "Greater Darwin"    = "NT",  "Rest of NT"  = "NT",
  "Australian Capital Territory" = "ACT"
)

plot_cells <- function(cells) {
  cells |>
    filter(region_name != "Other Territories") |>
    mutate(
      state = unname(state_of[region_name]),
      is_capital = region_name == "Australian Capital Territory" |
        startsWith(region_name, "Greater")
    ) |>
    ggplot() +
    geom_sf(aes(fill = state, alpha = is_capital), color = "white") +
    ggrepel::geom_text_repel(data = ~ sf::st_drop_geometry(.x) |> summarise(x0 = median(cx), y0 = median(cy), .by = c(region_name, state, is_capital)), aes(x = x0, y = y0, label = region_name, size = if_else(is_capital, 4.5, 3.5)), family = f1, fontface = "bold", bg.color = "black", color = "white", seed = 99) +
    scale_color_manual(values = pal_state, na.value = "red") +
    scale_fill_manual(values = pal_state, na.value = "red") +
    scale_alpha_manual(values = c(`FALSE` = 0.5, `TRUE` = 1)) +
    scale_size_identity() +
    theme_void(base_family = f1) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
}

p1 <- plot_cells(cells_q1) +
  labs(title = "Summer (Q1)") 

p2 <- plot_cells(cells_q3) +
  labs(title = "Winter (Q3)") 

p1 + p2 +
  plot_annotation(
      title = "Australia goes north for winter",
      subtitle = str_wrap("Share of all domestic holiday trips, 2014–2022, by region and quarter. Each square is an equal slice of the national total. Solid colour marks a capital city, faded the rest of the state. Between summer and winter, regional Queensland climbs from 14% to 23% while regional Victoria falls from 18% to 12%, and the Northern Territory's share grows almost sixfold.", width = 140),
      caption = "Source: Tourism Research Australia via the ecotourism R package · Graphic: Georgios Karamanis",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", family = f1),
        plot.subtitle = element_text(size = 13, family = f1),
        plot.caption = element_text(size = 10, family = f1, hjust = 0),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "grey99", color = NA)
      )
  )