library(tidyverse)
library(sf)
library(ggiraph)
library(patchwork)
library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 10, height = 8, dpi = 320)

attribution_studies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-12/attribution_studies.csv')

as_n <- attribution_studies |> 
  separate_longer_delim(iso_country_code, delim = ",") |> 
  count(iso_country_code, event_type, sort = TRUE) 

world <- read_sf("/Users/georgios/Documents/R/30daymapchallenge/2022/data/world.geo.json") |> 
  left_join(as_n, by = c("sov_a3" = "iso_country_code")) 

m <- ggplot(world, aes(fill = n, data_id = sovereignt, tooltip = sovereignt)) +
  geom_sf_interactive() +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

p <- ggplot(world) +
  geom_col(aes(y = sov_a3, x = n, fill = event_type)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

combined_plot <- m / p 

combined_plot

girafe(ggobj = combined_plot)