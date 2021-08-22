library(tidyverse)
library(ggimage)

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

main_characters <- c("Rachel Green", "Monica Geller", "Phoebe Buffay", "Joey Tribbiani", "Chandler Bing", "Ross Geller")

words <- friends %>% 
  filter(speaker %in% main_characters) %>% 
  mutate(
    words_n = str_count(text, "\\w+"),
    speaker = word(speaker)
    ) %>% 
  group_by(speaker, season) %>% 
  summarise(words_median = median(words_n))

images <- words %>% 
  distinct(speaker) %>% 
  mutate(img = paste0("2020-week37/images/", tolower(speaker), ".png"))

pal <- c("#EC5244", "#FADC4A", "#5CA1D1", "#9A6324", "#FEF590", "#4363d8")

ggplot(words) +
  geom_segment(data = images, aes(x = speaker, xend = speaker, y = -0.5, yend = 10, color = speaker), size = 1) +
  geom_point(aes(x = speaker, y = season, size = words_median, color = speaker)) +
  geom_text(aes(x = speaker, y = season, label = words_median), color = "grey10", family = "IBM Plex Mono Bold", size = 5) +
  geom_point(data = images, aes(x = speaker, y = -0.4, color = speaker), size = 25) +
  geom_image(data = images, aes(x = speaker, y = -0.4, image = img), size = 0.095, asp = 0.95) +
  labs(title = "Median number of words\nin a line of dialogue by season") +
  annotate("text", x = 6.7, y = 9, hjust = 0, label = "Source: friends R package / Graphic: Georgios Karamanis", angle = 90, color = "grey97", family = "Friends", size = 3) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = 1:10) +
  scale_size_continuous(range = c(10, 15)) +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA),
    axis.text.y = element_text(family = "Friends", color = "grey97"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "Friends", color = "grey97", size = 20, margin = margin(0, 0, 20, 0), hjust = 0.5)
  ) 

ggsave(here::here("temp", paste0("friends-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 8, height = 9)
