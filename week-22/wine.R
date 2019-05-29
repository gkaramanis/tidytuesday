library(tidyverse)
library(tidytext)
library(ggrepel)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wineWords <- wine_ratings %>% 
  select(-c("X1")) %>% 
  distinct() %>% 
  # group_by(taster_name) %>% 
  # filter(!is.na(taster_name)) %>% 
  select(taster_name, description) %>%
  unnest_tokens(word, description) %>%    
  anti_join(stop_words)

wineWords %>%
  filter(!is.na(taster_name)) %>% 
  group_by(taster_name, word) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(freq)) %>%
  group_by(taster_name) %>%
  slice(seq_len(3)) %>% 
  ggplot() +
  geom_text_repel(aes(label = word, color = word,
                 x = freq, y = taster_name)) +
  theme_minimal() +
  theme(
    text = element_text(family = "IBM Plex Sans", size = 8)
  )

ggsave("wine.png")  
  
