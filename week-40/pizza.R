library(tidyverse)
library(here)
library(ggpubr)

pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")
pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
# pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

pizza <- inner_join(pizza_jared, pizza_barstool, by = c("place" = "name")) %>% 
  filter(
    city == "New York" &
      place != "Joe's Pizza" &
      place != "Kiss My Slice" &
      place != "Little Italy Pizza"
         ) %>% 
  select(place, review_stats_all_count, review_stats_all_average_score) %>% 
  distinct() %>% 
  group_by(place) %>% 
  mutate(
    tomato = list(seq(1, review_stats_all_count, 1))
  ) %>% 
  unnest(tomato)

ggplot(pizza) +
  geom_point(aes(0, 0, size = review_stats_all_average_score), color = "#ffc87b") +
  geom_jitter(aes(0, 0), size = 0.1, height = 0.01, width = 0.01) +
  coord_fixed() +
  scale_size(range = c(3, 20)) +
  facet_wrap(vars(place)) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(
    here::here("week-40", "figures", "temp", paste0("pizza-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )

