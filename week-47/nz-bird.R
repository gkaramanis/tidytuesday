library(tidyverse)
library(here)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")








  ggsave(
    here::here("week-47", "plots", "temp", paste0("nzbird-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7, width = 17
  )
