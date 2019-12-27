library(tidyverse)
library(genius)
library(here)
library(tidytext)
library(lubridate)

# These line are to create my own dataset with lyrics, I couldn't figure out the original :)
# christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")
# 
# lyrics_book <- christmas_songs %>%
# mutate(dec_date = decimal_date(mdy(weekid))) %>%
#   select(song, performer, decimal_date()) %>% 
#   distinct(song, performer, .keep_all = TRUE) %>%
#   add_genius(performer, song, type = "lyrics")
# 
# write_csv(lyrics_book, here::here("week-52", "data", "lyrics-book.csv"))

# Read in lyrics dataset
lyrics_book <- read_csv(here::here("week-52", "data", "lyrics-book.csv")) 

# Collapse lyrics to one line per song
collapsed_lyrics <- lyrics_book %>%
  group_by(song, performer) %>% 
  mutate(song_lyrics = paste0(lyric, collapse = " ")) %>% 
  ungroup() %>% 
  # Keep these columns, keep unique
  select(dec_date, performer, track_title, song_lyrics) %>%
  distinct() %>% 
  # Filter out instrumental song and others, then number songs
  filter(song_lyrics != "Instrumental") %>% 
  filter(track_title != "Auld Lang Syne") %>% 
  filter(track_title != "Green Chri$tma$") %>% 
  mutate(song_nr = row_number())

blocks <- collapsed_lyrics %>% 
  # Tokenize
  unnest_tokens(word, song_lyrics) %>% 
  # Color
  mutate(
    chars = nchar(word),
    wordcolor = case_when(
      str_detect(word, "christmas") ~ "orangered2",
      str_detect(word, "santa") ~ "orangered2",
      str_detect(word, "carol") ~ "orangered2",
      str_detect(word, "noel") ~ "orangered2",
      str_detect(word, "sleigh") ~ "orangered2",
      # str_detect(word, "love") ~ "darkgreen",
      TRUE ~ "grey70"
    )
  )  %>% 
  # Calculate index of first and last character of every word
  mutate(wordstart = 1) %>% 
  group_by(song_nr) %>%
  mutate(
    wordstart = cumsum(lag(wordstart, default = 0) +
                         nchar(lag(word, default = 0))),
    wordend = wordstart + nchar(word)
  ) %>% 
  # Sort by year in Billboard
  group_by(song_nr) %>% 
  arrange(dec_date) %>% 
  mutate(dec_nr = group_indices())
  
ggplot(blocks) +
  geom_rect(aes(xmin = wordstart, ymin = dec_nr - 0.4,
                xmax = wordend, ymax = dec_nr + 0.4,
                fill = wordcolor), color = NA) +
  geom_text(aes(x = -10, y = dec_nr, label = paste0(track_title, " ", as.integer(dec_date))), hjust = 1, size = 2, check_overlap = TRUE) +
  scale_fill_identity() +
  scale_y_reverse() +
  coord_fixed(ratio = 30, xlim = c(-100, 3500)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text()
  ) +
  ggsave(
        here::here("week-52", "plots", "temp", paste0("christmas-lyrics-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 20, height = 10
        )
