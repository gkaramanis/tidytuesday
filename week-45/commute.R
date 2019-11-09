library(tidyverse)
library(here)
library(geofacet)
library(ggforce)

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

bike_state <- commute_mode %>% 
  filter(mode == "Bike") %>% 
  mutate(
    state = case_when(
      state == "Massachusett" ~ "Massachusetts",
      state == "Ca" ~ "California",
      TRUE ~ state
    )
  ) %>% 
  group_by(state) %>% 
  summarise(mean_bike = mean(percent), median_bike = median(percent)) %>% 
  mutate(x = list(c(1, 3, 2)), y = list(c(1.5, 1.5, 0)))


# palette: https://www.rwjf.org/en/library/infographics/visualizing-health-equity/_jcr_content/infographics/infographics.infographic.img.jpg/1503346670255.jpg


bike_state %>% 
  # filter(state == "Vermont") %>% 
ggplot() +
  geom_arc_bar(aes(x0 = 0.5, y0 = 0, r0 = 1, r = 1.1, start = 0, end = 2*pi * median_bike/5), fill = "blue", color = NA) +
  geom_arc_bar(aes(x0 = 3.5, y0 = 0, r0 = 1, r = 1.1, start = 0, end = 2*pi * median_bike/5), fill = "blue", color = NA) +
  geom_polygon(aes(x, y), fill = NA, color = "blue", size = 4) +
  coord_fixed(xlim = c(-1, 5), ylim = c(-1, 1.5)) +
  theme_void() +
  facet_geo(~ state) +
  ggsave(
    here::here("week-45", "plots", "temp", paste0("commute-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )

