library(tidyverse)
library(here)
library(gghighlight)

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

ggplot(safer_parks, aes(age_youngest, color = bus_type)) +
  geom_density() +
  gghighlight() +
  facet_wrap(vars(bus_type)) +
  theme(
    legend.position = "none"
  ) +
  ggsave(
  here::here("week-37", "figures", "temp", paste0("parks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 8, width = 18, dpi = 320
)
