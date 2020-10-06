library(tidyverse)

ggsave(here::here("temp", paste0("tournament-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)