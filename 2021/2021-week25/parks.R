library(tidyverse)

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

ggsave(here::here("temp", paste0("parks-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

