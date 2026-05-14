library(tidyverse)
library(geosphere)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 6, units = "in", dpi = 320)

cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/cities.csv')
links <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/links.csv')

city_rio <- cities |> 
  filter(id == "Q8678") |> 
  left_join(links, by = c("id" = "target")) |> 
  left_join(cities, by = c("source" = "id"))

world <- map_data("world") |> 
  filter(region != "Antarctica")

gc_paths <- city_rio |>
  mutate(pair_id = row_number()) |>
  select(pair_id, lng.x, lat.x, lng.y, lat.y) |>
  pmap_dfr(function(pair_id, lng.x, lat.x, lng.y, lat.y) {
    inter <- gcIntermediate(c(lng.x, lat.x), c(lng.y, lat.y), n = 100) |>
      as.data.frame()
    
    diff_of_lon <- abs(lng.x) + abs(lng.y)
    
    if (diff_of_lon > 180) {
      bind_rows(
        inter |> filter(lon >= 0) |> mutate(pair_id = paste0(pair_id, "_a")),
        inter |> filter(lon < 0)  |> mutate(pair_id = paste0(pair_id, "_b"))
      )
    } else {
      inter |> mutate(pair_id = as.character(pair_id))
    }
  })

ggplot(city_rio) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "grey9", fill = "royalblue4", linewidth = 0.2) +
  geom_point(aes(lng.y, lat.y), size = 0.5, color = "gold") +
  geom_path(data = gc_paths, aes(x = lon, y = lat, group = pair_id), color = "gold", linewidth = 0.35, alpha = 0.3) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey9", color = NA)
  )

record_polaroid()
