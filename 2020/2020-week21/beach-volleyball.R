library(tidyverse)
library(rnaturalearth)
library(gghighlight)
library(lubridate)
library(cowplot)
library(ggimage)

# Read in matches data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# Countries sf and centroids
countries_sf <- ne_countries(returnclass = "sf") %>% 
  select(name, geometry)
cc <- read_tsv(here::here("2020-week17", "data", "country-centroids.csv")) %>% 
  select(-country, country = name, longitude, latitude)

# Filter for FIVB tournaments, pivot_longer all players
player_travels <- vb_matches %>% 
  filter(circuit == "FIVB") %>% 
  pivot_longer(cols = c(w_player1, w_player2, l_player1, l_player2), names_to = "status", values_to = "player") %>% 
  distinct(date, country, player) %>% # remove duplicates after pivot
  group_by(player) %>% 
  arrange(date) %>%
  mutate(
    # Rename countries before matching with centroids
    country = case_when(
      country == "Korea" ~ "South Korea",
      country == "England" ~ "United Kingdom",
      TRUE ~ country
    )
  ) %>% 
  left_join(cc) %>% # match tournament country with centroids
  mutate(
    from_country = lag(country),
    from_country = replace_na(from_country, "no country") # keep value for table, "no country" won't be drawn on map
  ) %>% 
  left_join(cc, by = c(from_country = "country")) %>% # centroids for from-country
  rename(latitude = latitude.x, longitude = longitude.x, from_latitude = latitude.y, from_longitude = longitude.y) %>% 
  rowwise() %>% 
  mutate(
    from_ll = list(c(from_longitude, from_latitude)),
    to_ll = list(c(longitude, latitude))
  ) %>% 
  ungroup() %>% 
  add_count(player, name = "travels", sort = TRUE) # total travels by player

santos <- player_travels %>% 
  filter(player == "Ricardo Santos") %>%
  filter(country != from_country)

# Dataframe for table
santos_table <- santos %>% 
  mutate(
    year = year(date),
    month = month(date),
    month_f = month(date, label = TRUE, abbr = FALSE),
    year_d = decimal_date(date)
  ) %>% 
  group_by(year, month) %>% 
  mutate(travel_n = row_number()) %>% 
  ungroup() %>% 
  select(year, month, month_f, year_d, travel_n, country)

table_plot <- ggplot(santos_table) +
  # Manually highlight 2005, can't make it with gghightlight
  # all years but 2005
  geom_text(data = subset(santos_table, year != 2005), aes(x = year, y = -month - travel_n/4 + 0.25, label = country), hjust = 0, vjust = 0, family = "IBM Plex Sans", size = 1.5, colour = "grey50") +
  # 2005
  geom_text(data = subset(santos_table, year == 2005), aes(x = year, y = -month - travel_n/4 + 0.25, label = country), hjust = 0, vjust = 0, family = "IBM Plex Sans", size = 1.5, colour = "#df2407") +
  # months
  geom_text(aes(x = 1999.5, y = -month, label = month_f), hjust = 1, vjust = 0, family = "IBM Plex Sans Bold", size = 2, colour = "grey50") +
  # countries for all years but 2005
  geom_text(data = subset(santos_table, year != 2005), aes(x = year, y = 0, label = year), hjust = 0, vjust = 0, family = "IBM Plex Sans Bold", size = 2.5, colour = "grey50") +
  # countries for 2005
  geom_text(data = subset(santos_table, year == 2005), aes(x = year, y = 0, label = year), hjust = 0, vjust = 0, family = "IBM Plex Sans Bold", size = 2.5, colour = "#df2407") +
  scale_x_continuous(limits = c(1999, 2020)) +
  theme_void()

map_plot <- ggplot(santos) +
  geom_sf(data = countries_sf, aes(geometry = geometry), fill = "#f2f2f2", colour = "grey70", size = 0) +
  geom_point(aes(x = from_longitude, y = from_latitude), colour = "#df2407", size = 0.5) +
  geom_curve(aes(x = from_longitude, y = from_latitude, xend = longitude, yend = latitude), colour = "#df2407", size = 0.25, alpha = 1, curvature = 0.3) +
  gghighlight(year(date) == 2005, unhighlighted_params = list(alpha = 0.4)) +
  labs(
    title = "Ricardo Santos: The Most Traveled Beach Volleyball Player",
    subtitle = str_wrap("In order to participate in FIVB tournaments, Ricardo Santos has done more than 170 travels (traveling between different countries) from 2000 to 2019. Just in 2005, he traveled to 14 different countries.", 70),
    caption = "Data: Adam Vagnar | Photo: Petr Kadlec\nGraphic: Georgios Karamanis"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = "IBM Plex Serif") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle  = element_text(hjust = 0.5, size = 11.5, margin = margin(10, 0, 15, 0)),
    plot.caption = element_text(size = 5, hjust = 0.5, margin = margin(12, 0, 0, 0))
  )

ggdraw(map_plot) +
  draw_image(here::here("2020-week21", "img", "santos_50.png"), hjust = 0, x = -0.36, y = -0.05, scale = 0.7) +
  draw_plot(table_plot, x = 0.12, y = 0.07, height = 0.45, width = 0.9) 

ggsave(here::here("2020-week21", "plots", "temp", paste0("beach-volleyball-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7.5, width = 12)


