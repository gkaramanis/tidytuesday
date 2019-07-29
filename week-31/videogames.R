library(tidyverse)
library(here)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# unique(tolower())

# https://medium.com/nightingale/the-process-of-familiarity-an-interview-with-nicholas-rougeux-c30f1a1b2f8?source=rss----356ca48206e6---4

# ggsave(here("week-31", "img", paste0("videogames", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")))