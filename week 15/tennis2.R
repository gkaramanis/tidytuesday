library(tidyverse)
library(cheese)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

almostThere <- grand_slam_timeline %>%
  filter(., outcome == "Finalist" | outcome == "Won") %>% 
  group_by(player, outcome) %>% count()

ggplot(notThere) +
  geom_point(aes(year, player))
