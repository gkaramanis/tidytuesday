library(tidyverse)
library(here)

wdl3 <- wwc_outcomes %>%
  group_by(team) %>%
  mutate(
    w = sum(win_status == "Won")
    )