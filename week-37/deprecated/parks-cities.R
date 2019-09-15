library(tidyverse)
library(here)

# read in data
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv") %>% 
  mutate(acc_city = tolower(acc_city))

cities_coord <- readr::read_delim("https://raw.githubusercontent.com/zhangyongjiang/us-city-location-db/master/us-cities-location.csv", delim = "\t") %>% 
  magrittr::set_colnames(c("id", "latitude", "longitude", "city", "state")) %>% 
  mutate(city = tolower(city), longitude = -longitude)

acc_coord <- left_join(safer_parks, cities_coord,
                       by = c("acc_city" = "city", "acc_state" = "state")) %>% 
  count(longitude, latitude, age_youngest)

acc_coord %>% 
  filter(age_youngest < 19) %>% 
ggplot(aes(longitude, latitude)) +
  # geom_point(size = 2, alpha = 0.3) +
  stat_summary_2d(aes(z = n), bins = 100) +
  scale_fill_gradient(name = "reported accidents", low = "blue", high = "red") +
  ggsave("week-37/figures/-19.png")
