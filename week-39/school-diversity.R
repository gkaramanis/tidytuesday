library(tidyverse)
library(janitor)
library(here)

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

clean_school <- school_diversity %>% 
  clean_names() 

alf = 0.1

ggplot(clean_school) +
  geom_rect(aes(xmin = -0.1, ymin = 0.1, xmax = -0.1 - white, ymax = 0 + white), alpha = alf) +
  geom_rect(aes(xmin = 0.1, ymin = 0.1, xmax = 0.1 + black, ymax = 0.1 + black), alpha = alf) +
  geom_rect(aes(xmin = 0.1, ymin = -0.1, xmax = 0.1 + asian, ymax = -0.1 - asian), alpha = alf) +
  geom_rect(aes(xmin = -0.1, ymin = -0.1, xmax = -0.1 - hispanic, ymax = -0.1 - hispanic), alpha = alf) +
  coord_fixed() +
  facet_wrap(vars(school_year)) +

    ggsave(
    here::here("week-39", "figures", "temp", paste0("school-diversity", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )
