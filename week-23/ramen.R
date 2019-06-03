library(tidyverse)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
# dupes <- ramen_ratings %>% group_by(brand, variety, style, country, stars) %>% filter(n() > 1)

shio <- ramen_ratings %>%
  filter(country == "Japan", str_detect(variety, "Shio")) %>% 
  mutate(n = 1:n())

ggplot(shio, aes(n, stars)) +
  geom_col() +
  scale_y_reverse(limits = (0, 7)) +
  theme(
  )
