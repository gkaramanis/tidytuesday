library(httr)
library(jsonlite)
library(tidyverse)

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

titles <- nominees %>% 
  distinct(title) %>% 
  pull()

api_key <- "XXXXX"
url <- "https://api.themoviedb.org/3/search/tv"
gurl <- "https://api.themoviedb.org/3/genre/tv/list"

genres_res <- GET(gurl, query = list(api_key = api_key))
genres <- fromJSON(rawToChar(genres_res$content)) %>% 
  as.data.frame() %>% 
  janitor::clean_names()

tmdb_shows <- function(x) {
  res <- GET(url, query = list(api_key = api_key, query = x))
  shows <- fromJSON(rawToChar(res$content)) 
  df <- shows$results
  if(shows$total_results > 0) {
    df$original_title <- x
    print(x)
    rbind(df)
  }
  }

shows_df <- map_df(titles, tmdb_shows)

write_rds(shows_df, here::here("2021", "2021-week38", "data", "shows_df.rds"))

show_genres <- shows_df %>% 
  filter(original_title == name) %>% 
  group_by(name) %>% 
  slice_max(order_by = popularity, n = 1)
  
com_dram <- show_genres %>% 
  filter(lengths(genre_ids) > 0) %>% 
  unnest(genre_ids) %>% 
  left_join(genres, by = c("genre_ids" = "genres_id")) %>% 
  filter(genres_name == "Comedy" | genres_name == "Drama")

write_rds(show_genres, here::here("2021", "2021-week38", "data", "show_genres.rds"))
