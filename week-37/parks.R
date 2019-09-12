library(tidyverse)
library(here)
library(humapr)
library(fuzzyjoin)

tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

body_parts <- read_csv(here("week-37", "data", "body-parts.csv"))

injuries <- tx_injuries %>% 
  # select(age, body_part) %>% 
  mutate(
    body_part = tolower(body_part)
  ) %>% 
  separate_rows(body_part, sep = "(, )|(& )|(/)|( and )") %>%
  drop_na()

parts_match <- injuries %>% stringdist_inner_join(body_parts, by = c(body_part = "loc"))

humap() +
  geom_body(aes(loc = loc), parts_match)

ggsave(
  here::here("week-37", "figures", "temp", paste0("parks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 8, width = 18, dpi = 320
)
