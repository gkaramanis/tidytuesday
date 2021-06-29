library(tidyverse)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

ggsave(here::here("temp", paste0("animal-rescues-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

