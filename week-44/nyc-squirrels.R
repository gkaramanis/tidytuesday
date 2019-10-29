library(tidyverse)
library(here)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

squirrels_height <- nyc_squirrels %>% 
  mutate(
    height = case_when(
      above_ground_sighter_measurement == FALSE ~ 0,
      TRUE ~ as.numeric(above_ground_sighter_measurement)
    )
  ) %>% 
  select(long, lat, height) %>% 
  filter(!is.na(height))

ggplot(squirrels_height, aes(x = long, y = height, color = factor(height))) +
  geom_point() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")

  ggsave(
    here::here("week-44", "plots", "temp", paste0("nyc-squirrels-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )

