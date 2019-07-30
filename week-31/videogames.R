library(tidyverse)
library(here)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

over100_games <- video_games %>%
  filter(price > 100) %>% 
  mutate(game = case_when(
    startsWith(game, "Home Architect") ~ "Home Architect Home Architect - Ultimate Edition",
    startsWith(game, "PowerDirector") ~ "PowerDirector 17 Ultimate",
    T ~ game
  ))

ggplot(over100_games) +
  geom_col(aes(fct_rev(game), price)) +
  coord_flip() +
  
  ggsave(here("week-31", "img", paste0("videogames", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")))

# https://medium.com/nightingale/the-process-of-familiarity-an-interview-with-nicholas-rougeux-c30f1a1b2f8?source=rss----356ca48206e6---4

