library(tidyverse)
library(janitor)
library(here)
library(ggforce)

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

tri <- school_diversity %>% 
  clean_names() %>%
  head(1)

ggplot(tri) +
  geom_rect(aes(x = )) +
  
    ggsave(
    here::here("week-39", "figures", "temp", paste0("school-diversity", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )
