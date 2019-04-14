library(tidyverse)
library(wesanderson)

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# WINS/LOSSES ONLY IN FINALS!
winRatio <- as.tibble(grand_slam_timeline) %>%
  filter(., outcome == "Finalist" | outcome == "Won") %>% 
  group_by(player) %>%
  transmute(winner = sum(outcome == "Won"),
            finalist = sum(outcome == "Finalist"),
            ratio = winner / (winner + finalist),
            total = winner + finalist,
            hjust = if_else(winner > finalist, 0, 1),
            yjust = if_else(winner > finalist, 0.1, -0.1)) %>% 
  ungroup() %>% 
  unique %>%
  top_n(., 20, total) %>%
  arrange(., desc(total))

winRatio$player <- reorder(winRatio$player, winRatio$total)
winRatio$ratio <- round(winRatio$ratio*100, digits=0)

pal <- wes_palette("BottleRocket2")

ggplot(winRatio, aes(x = player)) +
  geom_bar(aes(y = winner), stat = "identity", fill = pal[4], width = 0.1) +
  geom_point(aes(y = winner), size = 2.5, color = pal[4]) +
  geom_bar(aes(y = -finalist), stat = "identity", fill = pal[2], width = 0.1) +
  geom_point(aes(y = -finalist), size = 2.5, color = pal[2]) +
  annotate("text",
           x = winRatio$player,
           y = winRatio$winner,
           label = winRatio$winner,
           size = 1,
           color = "white",
           family = "IBM Plex Mono") +
  annotate("text",
           x = winRatio$player,
           y = -winRatio$finalist,
           label = winRatio$finalist,
           size = 1,
           color = "white",
           family = "IBM Plex Mono") +
  annotate("text",
           x = winRatio$player,
           y = winRatio$yjust,
           label = winRatio$player,
           size = 1,
           color = "white",
           family = "IBM Plex Mono",
           fontface = "bold",
           hjust = winRatio$hjust) +
  coord_flip(ylim = c(-25, 25)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = pal[3])
  )

ggsave("./week 15/tennis2.png", dpi = 600, height = 2, width = 6)
