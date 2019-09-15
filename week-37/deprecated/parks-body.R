library(tidyverse)
library(here)
library(ggridges)
library(fuzzyjoin)

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

body_parts <- data.frame(loc = c("head", "neck", "shoulder", "arm", "elbow", "forearm", "wrist", "hand", "fingers", "back", "flank", "chest", "abdomen", "buttocks", "genitalia", "hip", "thigh", "knee", "leg", "ankle", "foot", "toes")) %>% 
  mutate(loc = factor(loc, loc))

injuries <- tx_injuries %>% 
  mutate(
    age = na_if(age, "n/a"),
    age = as.numeric(age),
    body_part = tolower(body_part)
  ) %>% 
  separate_rows(body_part, sep = "(, )|(& )|(/)|( and )") %>%
  drop_na() %>% 
  filter(age > 0) 

parts_match <- injuries %>% stringdist_inner_join(body_parts, by = c(body_part = "loc"), max_dist = 2) %>% 
  distinct(loc, injury_report_rec, age)

ggplot(parts_match) +
  # geom_density(alpha=.3) + 
  geom_density_ridges(aes(x = age, y = loc)) +
  # coord_flip() +
  theme(
    legend.position = "none"
  ) +
  ggsave(
  here::here("week-37", "figures", "temp", paste0("parks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 8, width = 18, dpi = 320
)
