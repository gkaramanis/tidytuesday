library(tidyverse)
library(lubridate)
library(wesanderson)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

pal <- wes_palette("Zissou1", 10, type = "continuous")

# my week 12 "policing.R" for inspiration
bird_collisions %>%
  ggplot(aes(date, reorder(genus, desc(genus)))) +
  geom_point(aes(alpha = 0.1, colour = factor(species))) +
  scale_fill_gradientn(colours = pal) +
  theme_dark() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "gray20")
  )
