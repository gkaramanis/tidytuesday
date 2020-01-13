library(tidyverse)
library(here)
# tidylog, conflicted always last!
library(tidylog)
library(conflicted)

theme_set(
  theme_minimal() +
    theme(legend.position = 'none')
  )


  ggsave(
    here::here("2020-week03", "plots", "temp", paste0("x-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320
  )

