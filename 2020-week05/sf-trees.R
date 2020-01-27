library(tidyverse)
library(here)


+ ggsave(here::here("2020-week05", "plots", "temp", paste0("sf-trees-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 8
)

