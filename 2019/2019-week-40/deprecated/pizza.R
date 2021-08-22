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
  select(place, review_stats_all_count, review_stats_all_average_score, review_stats_dave_average_score) %>% 
  distinct() %>% 
  group_by(place) %>% 
  mutate(
    tomatox = list(runif(log(review_stats_all_count), -review_stats_all_average_score/20, review_stats_all_average_score/20)),
    tomatoy = list(runif(log(review_stats_all_count), -review_stats_all_average_score/20, review_stats_all_average_score/20))) %>% 
  unnest(c(tomatox, tomatoy)) %>%
  group_by(place) %>% 
  mutate(
    pepperx = list(runif(log(review_stats_dave_average_score), -review_stats_all_average_score/20, review_stats_all_average_score/20)),
    peppery = list(runif(log(review_stats_dave_average_score), -review_stats_all_average_score/20, review_stats_all_average_score/20))
  ) %>% 
  unnest(c(pepperx, peppery))

pizza %>%
  # filter(place == "Arturo's") %>%
ggplot() +
  geom_point(aes(0, 0, size = review_stats_all_average_score), color = "#ffc87b") +
  geom_point(aes(tomatox, tomatoy), size = 5, color = "red", alpha = 0.7) +
  geom_text(aes(pepperx, peppery, label = "/"), color = "green", alpha = 0.7) +
  #coord_fixed() +
  scale_size(range = c(5, 30)) +
  xlim(-1, 1) +
  ylim(-1, 1) +
  facet_wrap(vars(place)) +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

ggsave(
    here::here("week-40", "figures", "temp", paste0("pizza-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    )

