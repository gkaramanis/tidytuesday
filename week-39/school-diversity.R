library(tidyverse)
library(janitor)
library(here)

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

clean_school <- school_diversity %>% 
  clean_names() %>% 
  group_by(st, school_year) %>% 
  mutate(
    white = median(white),
    black = median(black),
    asian = median(asian),
    hispanic = median(hispanic),
    multi = median(multi)
  ) %>% 
  distinct(st, white, black, asian, hispanic, multi) %>% 
  filter(school_year == "2016-2017")


ggplot(clean_school, alpha = 1, size = 0.1) +
  geom_rect(aes(xmin = -10, ymin = 10, xmax = -10 - white, ymax = 0 + white), fill =  "red") +
  geom_rect(aes(xmin = 10, ymin = 10, xmax = 10 + black, ymax = 10 + black), fill =  "green") +
  geom_rect(aes(xmin = 10, ymin = -10, xmax = 10 + asian, ymax = -10 - asian), fill =  "blue") +
  geom_rect(aes(xmin = -10, ymin = -10, xmax = -10 - hispanic, ymax = -10 - hispanic), fill =  "purple") +
  geom_tile(aes(x = 0, y = 0, width = multi, height = multi), fill =  "brown") +
  coord_fixed(xlim = c(-110, 110), ylim = c(-110, 110)) +
  facet_wrap(vars(st)) +
  theme_void() +
  ggsave(
    here::here("week-39", "figures", "temp", paste0("school-diversity", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )
