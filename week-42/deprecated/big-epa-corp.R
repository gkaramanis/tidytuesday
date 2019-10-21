library(tidyverse)
library(here)
library(data.tree)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv") %>% 
  distinct(brand = make, model)

car_corp <- read_csv(here::here("week-42", "car-corp-long.csv"))

big_corp <- full_join(car_corp, big_epa_cars)

big_corp_tree <- big_corp %>% 
  mutate(path_string = paste(corp, brand, sep = "/")) %>% 
  select(path_string, model)

tree <- as.Node(big_corp)
plot(tree)
