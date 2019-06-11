library(tidyverse)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

ggplot(meteorites) + 
  geom_point(aes(long, lat))

ggsave("./week-24/meteorites.png", width = 5, height = 4)
