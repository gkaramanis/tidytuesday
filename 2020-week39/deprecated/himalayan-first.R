library(tidyverse)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
  
deaths <- members %>% 
  group_by(peak_name) %>% 
  summarise(
    deaths = sum(died),
    summits = sum(success),
    death_rate = deaths / (summits + deaths)
  ) %>% 
  filter(deaths > 0) %>% 
  arrange(-deaths)

ggplot(deaths) +
  geom_point(aes(deaths, summits)) +
  # coord_fixed() +
  theme_void() +
  ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
