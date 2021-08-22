library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggtext)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv') %>% 
  arrange(-price)

# Get countries in dataset
gdpr_countries <- gdpr_violations %>% 
  distinct(name) %>% 
  pull()

# Get sf objects, filter by countries in dataset
countries_sf <- ne_countries(country = c(gdpr_countries, "Czechia"), scale = "large", returnclass = "sf") %>% 
  select(name, geometry) %>% 
  mutate(name = replace(name, name == "Czechia", "Czech Republic"))

# Group fines by country, merge with sf
countries_map <- gdpr_violations %>% 
  group_by(name) %>% 
  mutate(
    price_sum = sum(price),
    price_label = case_when(
      round(price_sum / 1e6) > 0 ~ paste0(round(price_sum / 1e6), "M"),
      round(price_sum / 1e5) > 0 ~ paste0(round(price_sum / 1e6, 1), "M"),
      round(price_sum / 1e3) > 0 ~ paste0(round(price_sum / 1e3), "K"),
      price_sum > 0 ~ paste0(round(price_sum / 1e3, 1), " K"),
      TRUE ~ "0"
    )
    ) %>% 
  left_join(countries_sf) %>% 
  select(name, price_sum, price_label, geometry)

# Copied from https://developers.google.com/public-data/docs/canonical/countries_csv
centroids <- read_tsv("2020-week17/data/country-centroids.csv")

# Dataset for red "arrows" (to draw with geom_polygon)
price_arrows <- countries_map %>% 
  select(name, price_sum, price_label) %>% 
  left_join(centroids) %>%
  mutate(
    arrow_x = list(c(longitude - 0.25, longitude, longitude + 0.25, longitude)),
    arrow_y = list(c(latitude - 0.03, latitude, latitude - 0.03, latitude + price_sum/1.5e6))
  ) %>% 
  unnest(c(arrow_x, arrow_y))

ggplot() +
  # map
  geom_sf(data = countries_map, aes(geometry = geometry), fill = "#EBE9E1", colour = "grey70", size = 0.25) +
  # country name
  geom_text(data = price_arrows, aes(x = longitude - 0.2, y = latitude - 0.4, label = name), check_overlap = TRUE, family = "IBM Plex Sans", hjust = 0, vjust = 1, size = 3.5) +
  # red price, over 10M
  geom_text(data = subset(price_arrows, price_sum > 10e6), aes(x = longitude - 0.2, y = latitude - 1.1, label = price_label), check_overlap = TRUE, family = "IBM Plex Sans Bold", hjust = 0, vjust = 1, size = 3.5, colour = "#BA4E35")  +
  # black price, under 10M
  geom_text(data = subset(price_arrows, price_sum < 10e6), aes(x = longitude - 0.2, y = latitude - 1.1, label = price_label), check_overlap = TRUE, family = "IBM Plex Sans Bold", hjust = 0, vjust = 1, size = 3.5, colour = "black")  +
  # red arrows
    geom_polygon(data = price_arrows, aes(x = arrow_x, y = arrow_y, group = name), fill = "#BA4E35", colour = NA, alpha = 0.8) +
  # title and caption
  annotate("richtext", x = -26, y = 80, hjust = 0, vjust = 1,
           label = "**Total amount of GDPR fines<br>by country**<br><span style = 'font-size:12pt'>Fine prices rounded to nearest million or thousand euro</span><br><span style = 'font-size:8pt'>Source: Privacy Affairs | Graphic: Georgios Karamanis</span>",
           family = "IBM Plex Serif", size = 8, lineheight = 1.1, fill = NA, label.color = NA) +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20)
  ) +
  coord_sf(xlim = c(-27.5, 37.5), ylim = c(32.5, 82.5), expand = FALSE) 

ggsave(here::here("2020-week17", "plots", "temp", paste0("gdpr-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 11)

