library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  mutate(nume = as.numeric(str_sub(prize_share, 1, 1)),
         deno = as.numeric(str_sub(prize_share, -1)),
         share = nume/deno,
         year = prize_year %% 10,
         decade = prize_year - 1900 - year) %>% 
  ggplot() + 
  geom_bar(
    aes(x = "", y = share),
    stat = "identity", show.legend = FALSE
  ) +
  # coord_polar("y", start = 0) +
  facet_grid(decade ~ year, switch = "y") 
  # scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  # scale_y_reverse(labels = seq(1900, 2020, by = 10),
  #                 breaks = seq(0, 120, by = 10))

ggplot(z, aes(x=prize_year, fill = deno)) +
  geom_bar(aes(x = prize_year, y = share),
           stat = "identity", show.legend = FALSE) +
  facet_grid(category ~ .)
  
  
  
  

