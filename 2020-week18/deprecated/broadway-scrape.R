library(rvest)
library(tidyverse)

url <- "https://www.broadway.org/broadway-theatres"

webpage <- read_html(url)

broadway_type <- html_nodes(webpage, ".theatre-descr div input") %>% 
  html_attrs() %>% 
  pluck(2) %>% 
  unlist()

broadway_value <- html_nodes(webpage, ".theatre-descr div input") %>% 
  html_attrs() %>% 
  pluck(3) %>% 
  unlist()

broadway_theatres <- tibble(type = broadway_type, value = broadway_value) %>% 
  mutate(n = rep(1:41, each = 4)) %>%
  pivot_wider(names_from = type, values_from = value) %>% 
  select(theatre = name, address = address1, longitude, latitude) %>% 
  mutate(
    theatre = as.character(theatre),
    address = as.character(address),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
    )

write_csv(broadway_theatres, here::here("2020-week18", "data", "broadway-theatres.csv"))
