library(tidyverse)
library(lubridate)
library(sf)
library(cowplot)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

theatres <- read.csv(here::here("2020-week18", "data", "broadway-theatres.csv"))

manhattan <- read_sf(here::here("2020-week18", "data", "manhattan_shapefile_from_qgis", "manhattan_shapefile_from_qgis.shp"))

top5_year <- grosses %>% 
  filter(show != "The Marriage of Kitty") %>% 
  mutate(
    year = year(week_ending),
    theatre = case_when(
      theatre == "Studio 54" ~ "Studio 54 Theatre",
      TRUE ~ theatre
      ),
    current_name = case_when(
      str_starts(theatre, "Foxwoods") ~ "Lyric Theatre",
      str_starts(theatre, "Ford") ~ "Lyric Theatre",
      str_starts(theatre, "Royale") ~ "Bernard B. Jacobs Theatre",
      str_starts(theatre, "Plymouth") ~ "Gerald Schoenfeld Theatre",
      str_starts(theatre, "Biltmore") ~ "Samuel J. Friedman Theatre",
      str_starts(theatre, "Vivian") ~ "Vivian Beaumont Theatre",
      str_starts(theatre, "46th") ~ "Richard Rodgers Theatre",
      str_starts(theatre, "Virginia") ~ "August Wilson Theatre",
      str_starts(theatre, "Martin") ~ "Al Hirschfeld Theatre",
      str_starts(theatre, "Gershwin") ~ "George Gershwin Theatre",
      str_starts(theatre, "Sam") ~ "Shubert Theatre",
      TRUE ~ theatre
      )
    ) %>% 
  group_by(theatre, year) %>% 
  mutate(capacity_median = median(pct_capacity)) %>% 
  ungroup() %>% 
  distinct(theatre, current_name, year, capacity_median) %>% 
  arrange(year, -capacity_median) %>%
  group_by(year) %>% 
  top_n(5, capacity_median) %>% 
  mutate(year_rank = row_number()) %>% 
  ungroup() %>% 
  left_join(theatres, by = c("current_name" = "theatre")) %>% 
  mutate(
    longitude = ifelse(str_starts(theatre, "Criterion"), -73.9876947, longitude),
    latitude = ifelse(str_starts(theatre, "Criterion"), 40.7573099, latitude)
  )

stars_colour = "#ffdf00"
bg_colour = "#4c77ab"

ggplot(top5_year) +
  geom_sf(data = manhattan, fill = "#381452", colour = bg_colour, size = 0.6, alpha = 0.15) +
  geom_text(aes(-73.9855, 40.7731, label = year), size = 10, alpha = 0.2, colour = "#b6c8df", family = "Avenir Next Condensed Heavy") +
  geom_path(aes(longitude, latitude), alpha = 0.5, colour = stars_colour) +
  geom_point(data = subset(top5_year, year_rank != 1), aes(longitude, latitude), size = 1.5, shape = 21, stroke = 1, fill = stars_colour, colour = bg_colour) +
  geom_text(data = subset(top5_year, year_rank == 1), aes(longitude, latitude, label = "★"), family = "Apple Symbols", size = 8.5, colour = stars_colour) +
  coord_sf(xlim = c(-73.992, -73.979), ylim = c(40.755, 40.774)) +
  facet_wrap(vars(year), ncol = 12, strip.position = "bottom") +
  labs(
    title = "Broadway Theatres",
    subtitle = "Showing the top 5 theatres with the most tickets sold\n(as percentage of their capacity) by year ",
    caption = "Source: Playbill | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = bg_colour, colour = NA),
    plot.title = element_text(family = "Avenir Next Condensed Heavy", size = 34, hjust = 0.5, colour = "white"),
    plot.subtitle = element_text(family = "Avenir Next", size = 16, hjust = 0.5, colour = "white", margin = margin(5, 0, 20, 0)),
    plot.caption= element_text(family = "Avenir Next", size = 9, hjust = 0.5, colour = "white", margin = margin(15, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) + ggsave(here::here("2020-week18", "plots", "temp", paste0("broadway-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 13, height = 8)

# theatres_legend <- top5_year %>% 
#   filter(year_rank == 1) %>% 
#   distinct(theatre, current_name, longitude, latitude) %>% 
#   group_by(current_name) %>% 
#   mutate(oldnew_names = paste0(theatre, collapse = "\n")) %>% 
#   ungroup() %>% 
#   distinct(oldnew_names, longitude, latitude)
# 
# l <- ggplot(theatres_legend) +
#   geom_sf(data = manhattan, fill = "#381452", colour = bg_colour, size = 0.6, alpha = 0.25) +
#   geom_text(aes(longitude, latitude, label = "★"), family = "Apple Symbols", size = 7, colour = stars_colour) +
#   geom_text(aes(longitude, latitude, label = oldnew_names), size = 1.5, family = "IBM Plex Sans Condensed Light", hjust = 0, color = "white", nudge_x = 0.0005) +
#   coord_sf(xlim = c(-73.99, -73.98), ylim = c(40.755, 40.774)) +
#   theme_void(base_family = "IBM Plex Sans") +
#   theme(
#     strip.text = element_blank(),
#     plot.background = element_rect(fill = bg_colour, colour = NA),
#     plot.margin = margin(20, 20, 20, 20)
#   )





