library(tidyverse)
library(fuzzyjoin)

# Get all tweets
tweets <- readRDS(here::here("the other tidytuesday/data/data.rds"))

# Filter tweets with media
media_tweets <- tweets %>% 
  filter(!is.na(media_url)) 

# Write list of URLs to text file
media_tweets %>%
  pull(media_url) %>%
  write_lines(here::here("the other tidytuesday/img/media_url.txt"))

# -nc Skip existing
# -i Read from file
# -P save to directory
# --random-wait ? not sure if needed, previously downloaded in iCloud Desktop
# First cd to img/media, then:
# wget -nc -i media_url.txt --random-wait

nsfw_img <- data.frame(
  media_url = list.files(here::here("the other tidytuesday/img/nsfw/")),
  nsfw = TRUE
)

media_tweets_tagged <- media_tweets %>% 
  regex_left_join(nsfw_img)

media_tweets_plot <- media_tweets_tagged %>% 
  mutate(
    year = lubridate::year(created_at),
    yearday = lubridate::yday(created_at),
    yearweek = lubridate::isoweek(created_at)
  )  %>% 
  rowwise() %>% 
  mutate(
    nsfw = if_else("nsfw" %in% tolower(hashtags), TRUE, nsfw),
    nsfw = if_else("tittytuesday" %in% tolower(hashtags), TRUE, nsfw),
    nsfw = if_else("tiddietuesday" %in% tolower(hashtags), TRUE, nsfw),
    nsfw = if_else("nsfwtwt" %in% tolower(hashtags), TRUE, nsfw),
    nsfw = if_else("onlyfans" %in% tolower(hashtags), TRUE, nsfw),
    nsfw = if_else("horny" %in% tolower(hashtags), TRUE, nsfw)
  ) %>% 
  ungroup() %>% 
  count(year, yearweek, nsfw) %>% 
  mutate(nsfw = replace_na(nsfw, FALSE)) %>% 
  group_by(year, yearweek) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  filter(nsfw)

ggplot(media_tweets_plot) +
  geom_line(aes(yearweek, n, group = nsfw, color = nsfw)) +
  # ggstream::geom_stream(aes(yearday, n, fill = nsfw), type = "proportional") +
  # scale_color_manual(values = "purple", na.value = "grey80") +
  scale_fill_manual(values = c("grey20", "purple1"), na.value = "grey80") +
  facet_wrap(vars(year), ncol = 1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )
  
