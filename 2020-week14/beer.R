library(tidyverse)
library(ggforce)
library(here)

beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')

beer_exported <- beer_taxed %>% 
  filter(type == "For export") %>%
  mutate(
    x = month_current * cos(month/12 * 2 * pi),
    y = month_current * sin(month/12 * 2 * pi)
  )
  
ggplot(beer_exported) +
  annotate("point", 0, 0) +
  geom_bspline_closed(aes(x, y, group = year), fill = NA, colour = "gold") +
  coord_fixed() +
  facet_wrap(vars(year)) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "brown", colour = NA)
  ) +
  ggsave(here::here("2020-week14", "plots", "beer.png"), dpi = 320)
