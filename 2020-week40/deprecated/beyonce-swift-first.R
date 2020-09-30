library(tidyverse)
library(tidytext)

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

beyonce_love <- beyonce_lyrics %>% 
  unnest_tokens(word, line) %>% 
  mutate(
    chars = nchar(word),
    wordcolor = case_when(
      word == "love" ~ "orangered2",
      TRUE ~ "grey10"
    )
  ) %>% 
  mutate(wordstart = 1) %>% 
  group_by(song_name, song_line) %>% 
  mutate(
    wordstart = cumsum(lag(wordstart, default = 0) +
                         nchar(lag(word, default = ""))),
    wordend = wordstart + nchar(word)
  ) %>% 
  ungroup()

ggplot(beyonce_love) +
  geom_rect(aes(xmin = wordstart, ymin = song_line - 0.4,
                xmax = wordend, ymax = song_line + 0.4,
                fill = wordcolor), color = NA) +
  scale_fill_identity() +
  scale_y_reverse() +
  coord_fixed(ratio = 2) +
  facet_wrap(vars(song_name)) +
  theme_void() +
  ggsave(here::here("temp", paste0("beyonce-swift-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 20, height = 15)
