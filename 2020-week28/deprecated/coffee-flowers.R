library(tidyverse)
library(colorspace)
library(countrycode)
library(cowplot)

# Read in data ------------------------------------------------------------
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# Map plot ----------------------------------------------------------------
coffee_countries = c("Ethiopia", "Guatemala", "Brazil", "Peru", "United States (Hawaii)", "Indonesia", "China", "Costa Rica", "Mexico", "Uganda", "Honduras", "Taiwan", "Nicaragua", "Tanzania", "Kenya", "Thailand", "Colombia", "Panama", "El Salvador", "Japan", "Ecuador", "Puerto Rico", "Haiti", "Burundi", "Vietnam", "Philippines", "Rwanda", "Malawi", "Laos", "Zambia", "Myanmar", "Mauritius", "Ivory Coast", "India")

world_map <- map_data("world", region = coffee_countries)

world_map <- map_data("world") %>% 
  filter(region != "Antarctica") %>%
  mutate(coffee = if_else(region %in% coffee_countries, TRUE, FALSE))

continent_flower = data.frame(
  continent = c("Americas", "Africa", "Asia"),
  long = c(-125, 75, 175),
  lat = c(-25, -25, 25)
)


# Coffee qualities, median values by continent ----------------------------
coffee_qualities <- coffee_ratings %>%
  mutate(
    country = if_else(str_detect(country_of_origin, "Puerto"), "Puerto Rico", country_of_origin),
    continent = countrycode(country, origin = "country.name", destination = "continent")
  ) %>% 
  add_count(continent) %>% 
  filter(n > 10) %>% 
  filter(!is.na(variety) & variety != "Other") %>%
  group_by(continent) %>% 
  summarise(across(aroma:moisture, median))


# Outer flower ------------------------------------------------------------
flower_outer <- coffee_qualities %>% 
  rowwise() %>% 
	mutate(
		x_outer = list(c(seq(0, 12, length.out = 11))),
		y_outer = list(c(0, aroma, 0, flavor, 0, aftertaste, 0, acidity, 0, body, 0))
		) %>% 
  unnest(c(x_outer, y_outer))

# Inner flower ------------------------------------------------------------
flower_inner <- coffee_qualities %>% 
  rowwise() %>% 
  mutate(
    moisture = round(moisture / 0.12 * 10, 2),	
		x_inner = list(c(0:12)),
		y_inner = list(c(0, balance, 0, uniformity, 0, clean_cup, 0, sweetness, 0, cupper_points, 0, moisture, 0)),
		) %>%
		unnest(c(x_inner, y_inner))


# Flower plot -------------------------------------------------------------
flower_p <- ggplot() +
# outer -------------------------------------------------------------------
  geom_area(data = flower_outer, aes(x_outer, y_outer, group = continent),
            fill = "#E0E0EB", colour = darken("#E0E0EB", 0.2), size = 0.5) +
  geom_text(data = subset(flower_outer, y_outer != 0),
            aes(x_outer, y_outer + 1.5, label = round(y_outer, 1)),
            family = "DIN Condensed Bold", size = 3.5, colour = "#3a2306") +
# inner -------------------------------------------------------------------
  geom_area(data = flower_inner, aes(x_inner, 0.5 * y_inner, group = continent),
            fill = "#E6D4CC", colour = darken("#E6D4CC", 0.2), size = 0.5) +
  geom_text(data = subset(flower_inner, y_inner != 0),
            aes(x_inner, 0.5 * y_inner + 1.25, label = round(y_inner, 1)),
            family = "DIN Condensed Bold", size = 3, colour = "#3a2306") +
# scales, theme, etc ------------------------------------------------------
  scale_y_continuous() +
  facet_wrap(vars(continent)) +
  coord_polar() +
  theme_void(base_family = "DIN Condensed Bold", base_size = 18) +
  theme(
    strip.text = element_text(colour = "#c19d67"),
    plot.background = element_rect(fill = "#6f4e37", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )


ggplot() +
# map ---------------------------------------------------------------------
  geom_polygon(data = world_map, aes(long, lat, group = group, fill = coffee)) +
  scale_fill_manual(values = c(darken("#6f4e37", 0.3), "#c19d67")) +
# flowers -----------------------------------------------------------------
  coord_fixed() +
  # theme_void() +
  theme(
    legend.position = "none",
    # plot.background = element_rect(fill = "#6f4e37", colour = NA)
  ) +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 11, height = 9)
# Ratings for n > 10 !











