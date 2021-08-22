library(tidyverse)
library(gt)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

conf_champs <- tournament %>% 
  filter(tourney_finish == "Champ") %>% 
  select(school, year, full_w, full_l, full_percent) %>%
  group_by(school) %>% 
  add_count() %>% 
  mutate(group_col = if_else(n > 5, 1, 2)) %>% 
  ungroup()

conf_champs1 <- conf_champs %>% 
  filter(group_col == 1)

conf_champs2 <- conf_champs %>% 
  filter(group_col == 2)

conf_champs12 <- full_join(conf_champs1, conf_champs2, by = "")

gt(conf_champs, groupname_col = "school")
