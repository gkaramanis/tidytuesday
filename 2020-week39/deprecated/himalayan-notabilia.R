library(tidyverse)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

a = 1.5

deaths_summits <- members %>% 
  group_by(peak_name, year) %>% 
  mutate(
    deaths_year = sum(died),
    summits_year = sum(!died & success) # number of people
    ) %>% 
  distinct(year, peak_name, deaths_year, deaths_year, summits_year) %>% 
  ungroup() %>% 
  group_by(peak_name) %>% 
  arrange(year) %>% 
  mutate(
    angle = if_else(deaths_year > 0, -a * deaths_year * pi/180, a * pi/180),
    a = cumsum(angle + lag(a, default = 0)) + pi / 2,
    x = 0,
    y = 0
  ) %>% 
  mutate(
    x = cumsum(lag(x, default = 0) + cos(lag(a, default = 0))) -1,
    y = cumsum(lag(y, default = 0) + sin(lag(a, default = 0))),
    deaths_total = sum(deaths_year),
    summits_total = sum(summits_year),
    death_rate = 100 * round(deaths_total / (deaths_total + summits_total), 3), # only summitted, not just alive
    a_total = sum(a)
  ) %>% 
  ungroup()

peak_labels <- deaths_summits %>% 
  filter(deaths_total > 20) %>% 
  group_by(peak_name) %>% 
  slice_tail()
  
ggplot(deaths_summits) +
  geom_path(aes(x = x, y = y, group = peak_name), stat = "unique", size = 0.3, lineend = "round", alpha = 0.7) +
  geom_text(data = peak_labels, aes(x = x, y = y, label = paste0(peak_name, " ", deaths_total, " (", death_rate, ")"), angle = a_total - 45), hjust = 0, nudge_x = 1, size = 2) +
  # scale_y_reverse() +
  coord_fixed(clip = "off") +
  theme_void() 

ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
