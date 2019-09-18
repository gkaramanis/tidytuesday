library(tidyverse)
library(here)
library(ggtern)

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

park_visits %>%
  filter(unit_type == "National Park" & year != "Total") %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_point(aes(year, visitors))
  # facet_wrap(vars(unit_name))

ggtern +
  geom_point(, aes(x, y, z))

 # save image
  ggsave(
  here::here("week-37", "figures", "temp", paste0("national-parks", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 12, width = 16, dpi = 320
  )
