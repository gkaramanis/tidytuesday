library(tidyverse)
library(here)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

ggsave(
      here::here("week-49", "plots", "temp", paste0("tickets-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 16
      )
 
