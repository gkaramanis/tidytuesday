library(tidyverse)
library(janitor)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

crops <- key_crop_yields %>% 
  clean_names() %>% 
  filter(!is.na(code)) %>% 
  rename_with(., ~ str_remove(.x, "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = wheat:bananas, names_to = "crop") %>% 
  filter(crop == "bananas") %>% 
  filter(year > 2010) %>%
  filter(!is.na(value)) %>% 
  mutate(entity = fct_reorder(entity, value, .desc = TRUE))

ggplot(crops) +
  geom_col(aes(entity, value)) +
  facet_wrap(vars(year), ncol = 1) +
  ggsave(here::here("temp", paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 10)
