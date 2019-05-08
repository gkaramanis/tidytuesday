library(tidyverse)
library(nord)

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# WINS/LOSSES ONLY IN FINALS!
winRatio <- as.tibble(grand_slam_timeline) %>%
  filter(., outcome == "Finalist" | outcome == "Won") %>%
  group_by(player) %>%
  transmute(
    winner = sum(outcome == "Won"),
    finalist = sum(outcome == "Finalist"),
    ratio = winner / (winner + finalist),
    total = winner + finalist,
    hjust = if_else(winner > finalist, 0, 1),
    yjust = if_else(winner > finalist, 0.1, -0.1)
  ) %>%
  ungroup() %>%
  unique() %>%
  top_n(., 20, total) %>%
  arrange(., desc(total))

winRatio$player <- reorder(winRatio$player, winRatio$total)
winRatio$ratio <- round(winRatio$ratio * 100, digits = 0)

ggplot(winRatio, aes(x = player)) +
  geom_bar(aes(y = winner), stat = "identity", fill = "white", width = 0.1, alpha = 0.5) +
  geom_bar(aes(y = finalist), stat = "identity", fill = "white", width = 0.1, alpha = 0.5) +
  geom_point(aes(y = winner), size = 2.5, color = nord("victory_bonds")[4]) +
  geom_point(aes(y = finalist), size = 2.5, color = nord("victory_bonds")[1]) +
  annotate("text",
    x = winRatio$player,
    y = winRatio$winner,
    label = winRatio$winner,
    size = 1,
    color = "white",
    family = "IBM Plex Mono",
    fontface = "bold"
  ) +
  annotate("text",
    x = winRatio$player,
    y = winRatio$finalist,
    label = winRatio$finalist,
    size = 1,
    color = "white",
    family = "IBM Plex Mono",
    fontface = "bold"
  ) +
  # annotate("text",
  #   x = winRatio$player,
  #   y = winRatio$yjust,
  #   label = winRatio$player,
  #   size = 1.7,
  #   color = "gray20",
  #   family = "IBM Plex Serif",
  #   fontface = "bold",
  #   hjust = winRatio$hjust
  # ) +
  coord_flip() +
  labs(title = "Top-23 Players With Most Grand Slam Finals Appearances,\nby total finals",
       caption = toupper("Source: Wikipedia | Graphic: @geokaramanis")) +
  annotate(GeomRect, xmin = 2.5, ymin = 18, xmax = 6, ymax = 22.5, fill = "white") +
  annotate("text", x = 5, y = 20.5, label = "wins", size = 2.5, family = "IBM Plex Sans") +
  annotate(GeomPoint, x = 5, y = 19, size = 2.5, color = nord("victory_bonds")[4]) +
  annotate("text", x = 5, y = 19, label = "19", size = 1, color = "white", family = "IBM Plex Mono", fontface = "bold") +
  annotate("text", x = 3.5, y = 20.9, label = "losses", size = 2.5, family = "IBM Plex Sans") +
  annotate(GeomPoint, x = 3.5, y = 19, size = 2.5, color = nord("victory_bonds")[1]) +
  annotate("text", x = 3.5, y = 19, label = "7", size = 1, color = "white", family = "IBM Plex Mono", fontface = "bold") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,25)) +
  theme(
    panel.background = element_rect(fill = "gray90"),
    text = element_text(family = "IBM Plex Sans", size = 6),
    axis.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggsave("./week 15/tennis2.png", dpi = 600, height = 3, width = 4)
