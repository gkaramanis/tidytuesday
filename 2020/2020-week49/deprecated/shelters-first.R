library(tidyverse)
library(lubridate)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

occupancy <- shelters %>%
  mutate(
    occupancy_year = year(occupancy_date),
    occupancy_month = month(occupancy_date)
    ) %>% 
  group_by(occupancy_year, occupancy_month) %>% 
  mutate(
    total_occupancy = sum(occupancy, na.rm = TRUE),
    total_capacity = sum(capacity, na.rm = TRUE),
    occupancy_rate = total_occupancy/total_capacity
    ) %>% 
  ungroup() %>% 
  distinct(occupancy_year, occupancy_month, total_occupancy, total_capacity, occupancy_rate) %>% 
  arrange(occupancy_year, occupancy_month)

ggplot(occupancy) +
  geom_line(aes(occupancy_year + occupancy_month/12, occupancy_rate)) +
  ylim(0.8, 1) 

ggsave(here::here("temp", paste0("shelters-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

