library(tidyverse)
library(here)
library(ggimage)
library(gganimate)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

city_race <- big_epa_cars %>%
  pivot_longer(cols = c("city08", "cityA08"), names_to = "city_fuel", values_to = "city_mpg") %>%
  filter(city_mpg > 0) %>%
  group_by(make) %>%
  summarize(city_mpg_median = median(city_mpg)) %>%
  ungroup() %>%
  mutate(make = fct_reorder(make, city_mpg_median)) %>%
  top_n(10, city_mpg_median) %>%
  arrange(city_mpg_median) %>%
  mutate(lane = row_number() %% 2 * 2)

highway_race <- big_epa_cars %>%
  pivot_longer(cols = c("highway08", "highwayA08"), names_to = "highway_fuel", values_to = "highway_mpg") %>%
  filter(highway_mpg > 0) %>%
  group_by(make) %>%
  summarize(highway_mpg_median = median(highway_mpg)) %>%
  ungroup() %>%
  mutate(make = fct_reorder(make, highway_mpg_median)) %>%
  top_n(10, highway_mpg_median) %>%
  arrange(highway_mpg_median) %>%
  mutate(lane = row_number() %% 2 * 2 + 13) %>%
  group_by(highway_mpg_median) %>% 
     mutate(make = paste0(make, collapse = ", ")) 

# Icon by mynamepong, flaticon.com
car = here::here("week-42", "images", "car.png")

ggplot() +
  # city background 
  geom_rect(aes(xmin = -2, ymin = 0, xmax = 7, ymax = 105), fill = "grey60") +
  # city street
  geom_rect(aes(xmin = -0.75, ymin = 0, xmax = 2.75, ymax = 105), fill = "grey20") +
  # city street line
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = 105), size = 2, color = "grey90", linetype = 2) +
  # city label
  geom_text(aes(x = -1.6, y = 1, label = "CITY"), size = 8, color = "grey90", alpha = 0.4, family = "IBM Plex Sans Medium", angle = 90, hjust = "left") +
  
  # highway background
  geom_rect(aes(xmin = 8, ymin = 0, xmax = 17, ymax = 105), fill = "brown4") +
  # highway 
  geom_rect(aes(xmin = 12.25, ymin = 0, xmax = 15.75, ymax = 105), fill = "grey20") +
  # highway road line
  geom_segment(aes(x = 14, y = 0, xend = 14, yend = 105), size = 2, color = "grey90", linetype = "longdash") +
  # highway label
  geom_text(aes(x = 16.6, y = 1, label = "HIGHWAY"), size = 8, color = "grey90", alpha = 0.4, family = "IBM Plex Sans Medium", angle = 90, hjust = "left") +
  
  # city cars
  geom_image(data = city_race, aes(x = lane, y = city_mpg_median, image = car), size= .05, asp = 0.6) +
  # city cars make
  geom_text(data = city_race, aes(x = 6.6, y = city_mpg_median, label = make), hjust = "right", family = "IBM Plex Sans Condensed", size = 6, color = "white") +
  geom_segment(data = city_race, aes(x = 6.7, y = city_mpg_median, xend = 7, yend = city_mpg_median), color = "white") +
  
  # highway cars
  geom_image(data = highway_race, aes(x = lane, y = highway_mpg_median, image = car), size= .05, asp = 0.6) +
  # highway cars make
  geom_text(data = highway_race, aes(x = 8.4, y = highway_mpg_median, label = make), hjust = "left", family = "IBM Plex Sans Condensed", size = 6, color = "white", check_overlap = TRUE) +
  geom_segment(data = highway_race, aes(x = 8, y = highway_mpg_median, xend = 8.3, yend = highway_mpg_median), color = "white") +
  
  # y axis between roads
  geom_text(aes(x = 7.5, y = seq(0, 100, 10)), label = seq(0, 100, 10), family = "IBM Plex Mono Light", size = 6, color = "black") +
  geom_text(aes(x = 7.5, y = seq(5, 95, 10)), label = seq(5, 95, 10), family = "IBM Plex Mono Light", size = 6, color = "grey50") +
  geom_text(aes(x = 7.5, y = -3, label = "MPG/MPGe"), family = "IBM Plex Mono Light", size = 6, color = "black") +
  
  # title and theme
  xlim(-2, 17) +
  labs(
    title = "Tesla is the leading car brand in energy efficiency",
    subtitle = "Top 10 most energy efficient brands in city and highway driving.\nRanking is based on calculated median MPG and MPGe of all\nmodels made by every car manufacturer since 1984.",
    caption = "Source: EPA | Graphic: Georgios Karamanis\nCar icon by mynamepong, flaticon.com"
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Serif Bold", size = 28, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 24, margin = margin(10, 0, 0, 40)),
    plot.caption = element_text(size = 12, color = "grey60", margin = margin(0, 40, 0, 0)),
    
  ) +
  
  transition_time(year)

  #  ggsave(
  #   here::here("week-42", "figures", "temp", paste0("big-epa-cars-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  #   height = 19, width = 12, dpi = 320
  # )
