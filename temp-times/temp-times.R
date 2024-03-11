library(tidyverse)
library(lubridate)

# week = 21

# save --------------------------------------------------------------------
tmp_times <- list.files(here::here("temp")) %>%
  as_tibble() %>%
  mutate(datetime = ymd_hms(str_extract(value, "\\d{8}_\\d{6}"))) %>%
  separate(datetime, into = c("day", "time"), sep = " ") %>%
  mutate(day = as_date(day))

write_csv(tmp_times, here::here("temp-times", "data", paste0("temp-times-2021-", week, ".csv")))


