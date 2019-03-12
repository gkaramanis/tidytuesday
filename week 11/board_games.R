library(ggplot2)
library(dplyr)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")
board_games$words <- sapply(strsplit(board_games$name, " "), length)
board_games$chars <- nchar(gsub(" ", "", board_games$name))
board_games$charsPerWord <- board_games$chars/board_games$words

ggplot() +
   geom_point(data = board_games, 
              aes(x = year_published, y = chars),
              alpha = 0.06, color="midnightblue") +
  scale_x_continuous(minor_breaks = seq(1950, 2016, by = 1),
                     breaks = seq(1950, 2016, by = 10)) +
  expand_limits(y = 0) +
  labs(title = "Long game titles get longer with time",
       subtitle = "Length of game titles by year",
       caption = toupper("Source: boardgamegeek.com")) +
  ylab("Number of characters") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "cornsilk"),
    plot.title = element_text(size = 14, face = "bold", color = "gray10"),
    plot.subtitle = element_text(size = 12, color = "gray20"),
    plot.caption = element_text(color = "gray50"),
    axis.title.y = element_text(color="gray30"),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank()
    )
 
