library(rvest)
library(tidyverse)
library(janitor)
library(fuzzyjoin)
library(here)

url <- "https://en.m.wikipedia.org/wiki/List_of_popular_Christmas_singles_in_the_United_States"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

songs_wikipedia <- html_table(tbls[[3]]) %>% 
  clean_names() %>% 
  mutate(title = str_extract_all(title, '(?<=\")(.*)(?=\")')) %>% 
  mutate(song_id = paste0(title, artist)) %>% 
  select(everything(), release_year = year)

billboard <- read_csv("https://query.data.world/s/ce3mphfd2mx6kqgrwwcitat7wcby2j") %>% 
  clean_names() %>% 
  select(title = song, everything())

songs <- stringdist_left_join(billboard, songs_wikipedia, by = "song_id", max_dist = 1) %>% 
  select(song_id.x, song_id.y, title.x, title.y, everything()) %>% 
  filter(!is.na(song_id.y) & !is.null(title.y)) %>%
  as_tibble() %>%
  clean_names()

# write_csv(songs_wikipedia, here("week-52", "data", "songs-wikipedia.csv"))
# write_csv(billboard, here("week-52", "data", "billboard.csv"))
# write_csv(songs, here("week-52", "data", "christmas-songs.csv"))

write_rds(songs, here("week-52", "data", "christmas-songs.rds"))
