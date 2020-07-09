library(tidyverse)
library(lubridate)
library(ggridges)


# Read in coffee data -----------------------------------------------------
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv') %>% 
  mutate(grading_date = mdy(grading_date)) %>% 
  filter(total_cup_points > 0)

coffee_qualities <- coffee_ratings %>% 
  group_by(variety) %>% 
  mutate(across(aroma:cupper_points, median)) %>% 
  ungroup() %>% 
  pivot_longer(cols = aroma:cupper_points, names_to = "quality")

ggplot(coffee_qualities) +
  geom_line(aes(x = quality, y = value, group = variety)) +
  facet_wrap(vars(species)) +
  ylim(0, 10) +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
