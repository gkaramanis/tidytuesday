library(tidyverse)
library(fuzzyjoin)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')
shots <- read_csv(here::here("2020-week33", "data", "shots.csv"))

scene_shots <- avatar %>% 
  select(id:chapter_num) %>% 
  left_join(scene_description) %>% 
  regex_left_join(shots, by=c("scene_description" = "string")) %>% 
  distinct(id, shot, .keep_all = TRUE) %>% 
  filter(!is.na(shot)) %>% 
  count(book_num, chapter_num, shot) %>% 
  group_by(book_num, chapter_num) %>% 
  mutate(total = sum(n), freq = n / total)

ggplot(scene_shots) +
  geom_jitter(aes(chapter_num, book_num, colour = shot)) +
  ggsave(here::here("temp", paste0("avatar-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
