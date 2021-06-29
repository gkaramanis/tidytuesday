library(tidyverse)
library(lubridate)
library(ggfx)
library(ggimage)

tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

tweets_tt <- tweets %>% 
  mutate(tt = str_detect(tolower(content), "#tidytuesday")) %>% 
  filter(!is.na(tt)) %>% 
  group_by(tt) %>% 
  summarise(
    like_ratio = sum(like_count)/n(),
    retweet_ratio = sum(retweet_count)/n()
  ) %>% 
  ungroup()

img1 = here::here("2021", "2021-week24", "img", "layer1t.png")
img2 = here::here("2021", "2021-week24", "img", "layer2t.png")
img3 = here::here("2021", "2021-week24", "img", "layer3.png")

f1 = "Jefferies"
f2 = "Gill Sans Light"

ggplot(tweets_tt) +
  # Top area
  as_reference(
    annotate("polygon", x = c(0, 1, 1, 0), y = c(tweets_tt$like_ratio[1], tweets_tt$like_ratio[2], 29.9, 29.9), fill = "black"),
    id = "bg"
  ) +
  with_blend(
    geom_image(aes(x = 0.5, y = 15, image = img1), size = 1, asp = 0.8),
    bg_layer = "bg",
    blend_type = "in"
  ) +
  # Middle area
  as_reference(
    geom_area(aes(x = as.numeric(tt), y = like_ratio)),
    id = "area2"
  ) +
  with_blend(
    geom_image(aes(x = 0.53, y = 14.25, image = img2), size = 1.1, asp = 0.8),
    bg_layer = "area2",
    blend_type = "in"
  ) +
  # Bottom area
  as_reference(
    geom_area(aes(x = as.numeric(tt), y = retweet_ratio)),
    id = "area1"
  ) +
  with_blend(
    geom_image(aes(x = 0.45, y = 15, image = img3), size = 1, asp = 0.8),
    bg_layer = "area1",
    blend_type = "in"
  ) +
  geom_line(aes(x = as.numeric(tt), y = like_ratio), size = 1.75, color = "white") +
  geom_line(aes(x = as.numeric(tt), y = retweet_ratio), size = 1.75, color = "white") +
  geom_point(aes(x = as.numeric(tt), y = like_ratio), size = 8, color = "#4C6655") +
  geom_point(aes(x = as.numeric(tt), y = retweet_ratio), size = 8, color = "#4C6655") +
  annotate("text", x = c(0, 1), y = 29.5, label = c("no #TidyTuesday", "#TidyTuesday"), family = f1, size = 7) +
  annotate("text", x = -0.075, y = c(min(tweets_tt$like_ratio), min(tweets_tt$retweet_ratio)), label = c("likes", "retweets"), hjust = 1, family = f2, size = 5) +
  geom_text(aes(x = as.numeric(tt), y = like_ratio, label = round(like_ratio, 1)), family = f1, size = 3, color = "white") +
  geom_text(aes(x = as.numeric(tt), y = retweet_ratio, label = round(retweet_ratio, 1)), family = f1, size = 3, color = "white") +
  xlim(-0.25, 1.25) +
  scale_y_continuous(breaks = c(0, 10, 20), limits = c(0, 29.9)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "#DuBoisChallenge tweets",
    subtitle = "Ratio of likes and retweets to total number of tweets\nwith and without #TidyTuesday",
    caption = "Source: Anthony Starks, Allen Hillery Sekou Tyler · Graphic: Georgios Karamanis\nPhotos from The New York Public Library"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = f1),
    axis.title.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#D34150"),
    panel.grid.minor.y = element_line(color = "#D34150"),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "#EBCF53", color = NA),
    plot.title = element_text(family = f1, hjust = 0.5, size = 24),
    plot.subtitle = element_text(family = f2, hjust = 0.5, margin = margin(0, 0, 20, 0), size = 14),
    plot.caption = element_text(family = f1, hjust = 0.5, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("temp", paste0("dubois-tweets-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8.25, width = 6)

