library(tidyverse)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

trails <- hike_data %>% 
  mutate(name = str_trim(name)) %>% 
  separate(length, into = c("length", "roundtrip"), sep = ", ") %>% 
  mutate(
    length = parse_number(length) * 1609.34,  # miles to metres
    gain = parse_number(gain) * 0.3048,  # feet to metres
    highpoint = parse_number(highpoint) * 0.3048, # feet to meres
    rating = as.numeric(rating),
    location = str_remove(location, " -- .+")
  ) %>% 
  group_by(location) %>% 
  mutate(
    length = median(length),
    highpoint = median(highpoint),
    gain = median(gain)
    ) %>% 
  ungroup() %>% 
  distinct(location, length, gain, highpoint)

ggplot(trails) +
  geom_col(aes(x = location, y = highpoint), width = 0.5) +
  geom_tile(aes(x = location, y = gain, width = 0.7, height = 8)) 

ggsave(here::here("temp", paste0("washington-hiking-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

