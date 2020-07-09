library(tidyverse)
library(lubridate)
library(colorspace)
library(countrycode)


# Read in coffee data -----------------------------------------------------
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

ratings_date <- coffee_ratings %>%
	mutate(grading_date = mdy(grading_date))

country_ratings <- coffee_ratings %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(
    country = case_when(
      str_detect(country_of_origin, "Puerto") ~ "Puerto Rico",
      str_detect(country_of_origin, "Tanzania") ~ "Tanzania",
      str_detect(country_of_origin, "Cote") ~ "Ivory Coast",
      TRUE ~ country_of_origin
    ),
    continent = countrycode(country, origin = "country.name", destination = "continent")
  ) %>% 
  group_by(country) %>% 
  mutate(tcp_med = median(total_cup_points)) %>% 
  ungroup() %>% 
  distinct(country, continent, tcp_med) %>% 
  mutate(country = if_else(str_detect(country, "Hawaii"), "Hawaii", country))


ggplot(ratings_date) +
	geom_dotplot(aes(x = grading_date, fill = country_of_origin), stackdir = "center") +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 11, height = 9)












