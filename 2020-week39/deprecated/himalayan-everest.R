library(tidyverse)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

a = 0.1

everest <- members %>% 
  filter(peak_name == "Everest") %>% 
  group_by(year, expedition_id) %>% 
  mutate(deaths_exp = sum(died)) %>% 
  distinct(year, expedition_id, deaths_exp) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    angle = if_else(deaths_exp > 0, -a * pi/180, a * pi/180),
    a = cumsum(angle + lag(a, default = 0)) + pi / 2,
    x = 0,
    y = 0
  ) %>% 
  mutate(
    x = cumsum(lag(x, default = 0) + cos(lag(a, default = 0))) -1,
    y = cumsum(lag(y, default = 0) + sin(lag(a, default = 0))),
    ) %>% 
  ungroup() %>% 
  distinct(year, expedition_id, x, y, a) 
  
everest %>% 
  filter(year > 1900) %>% 
  ggplot() +
  geom_path(aes(x = year + x, y = y, group = year)) +
  # coord_fixed(clip = "off") +
  # theme_void() +
  ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
