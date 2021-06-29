library(tidyverse)
library(treemapify)

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

beer_medals <- beer_awards %>% 
  mutate(state_abbv = toupper(state)) %>% 
  count(year, category)

ggplot(beer_medals, aes(area = n, fill = category)) +
  geom_treemap() +
  coord_fixed() +
  facet_wrap(vars(year)) +
  theme(
    legend.position = "none"
  ) 

ggsave(here::here("temp", paste0("beer-awards-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

