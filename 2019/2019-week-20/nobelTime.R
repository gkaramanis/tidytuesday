library(tidyverse)

nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

# first try, but there are multiple papers for some authors ----
nobel_winner_all_pubs %>%
  filter(is_prize_winning_paper == "YES") %>% 
  mutate(yearsToPrize = prize_year - pub_year) %>% 
  arrange(desc(pub_year)) %>% 
  mutate(id = row_number()) %>% 
  ggplot() +
  # geom_point(aes(x = pub_year, y = id)) +
  # geom_point(aes(x = prize_year, y = id)) +
  geom_rect(aes(xmin = pub_year, xmax = prize_year,
                ymin = id, ymax = id + 1))

# find unique combinations, but losing papers ----
winP <- nobel_winner_all_pubs %>%
  filter(is_prize_winning_paper == "YES") %>% 
  mutate(yearsToPrize = prize_year - pub_year) %>% 
  arrange(desc(pub_year)) %>%
  distinct(laureate_id, prize_year, .keep_all = TRUE) %>% 
  mutate(id = row_number())
