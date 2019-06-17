library(here)
library(tidyverse)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

bird_counts %>%
  distinct(year, total_hours) %>% 
  ggplot(aes(year, total_hours)) +
  geom_line() +
  theme_minimal() +
  
  ggsave(here("week-25", "xmasBirds.png"))
