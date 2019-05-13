library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  filter(!is.na(death_country)) %>%
  ggplot() +
  geom_segment(aes(x = 1, y = birth_country,
                 xend = 2, yend = death_country),
               alpha = 0.3) +
  facet_grid(. ~ category) +
  theme(
    axis.text = element_blank()
  )

nobel_winners %>% 
  filter(!is.na(death_country)) %>% 
  mutate(diffCountry = ifelse(birth_country == death_country, 0, 1)) %>% 
  group_by(category) %>% 
  tally(diffCountry) %>% 
  arrange(desc(n))
         