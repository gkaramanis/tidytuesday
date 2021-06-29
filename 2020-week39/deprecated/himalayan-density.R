library(tidyverse)
library(cartography)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

members %>% 
  filter(season != "Unknown") %>% 
  ggplot() +
  geom_density_2d_filled(aes(x = year, y = death_height_metres)) +
  geom_density_2d(aes(x = year, y = death_height_metres), size = 0.2, color = "grey20") +
  ylim(4000, 8900) +
  scale_fill_manual(values = rev(carto.pal(pal1 = "taupe.pal", n1 = 14))) +
  facet_wrap(vars(season)) +
  # theme_void() +
  theme(legend.position = "none") 

ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 9)
