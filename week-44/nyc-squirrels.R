library(tidyverse)
library(here)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")



  ggsave(
    here::here("week-44", "plots", "temp", paste0("nyc-squirrels-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )

