library(tidyverse)
library(lubridate)
library(here)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear <- nuclear_explosions %>%
  # filter(year == 1961) %>% 
  mutate(
    yearday = yday(ymd(date_long)),
    weekday = wday(ymd(date_long)),
    month = month(ymd(date_long)),
    week = week(ymd(date_long)),
    day = day(ymd(date_long))
  ) %>% 
  add_count(date_long)

nuclear %>% 
  ggplot() +
  geom_tile(aes(weekday, day%%4, height = 0.9, width = 0.9, fill = n), show.legend = FALSE) +
  coord_fixed(expand = FALSE) +
  scale_y_reverse() +
  facet_grid(cols = vars(month), rows = vars(year)) +
  theme_minimal(
  )

