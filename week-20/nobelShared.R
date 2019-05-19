library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  mutate(shares = as.numeric(str_sub(prize_share, -1))) %>%
  mutate(year = prize_year %% 10) %>% 
  mutate(decade = prize_year - 1900 - year) %>%
  ggplot(aes(year, decade)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_reverse(labels = seq(1900, 2020, by = 10),
                  breaks = seq(0, 120, by = 10)) 

z <- nobel_winners %>%
  mutate(shares = as.numeric(str_sub(prize_share, -1))) %>%
  mutate(decade = prize_year - 1900) %>%
  mutate(year = prize_year %% 10) 
  
