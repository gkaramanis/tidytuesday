library(ggplot2)
library(dplyr)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>%
  arrange(tournament_date)

age_slams_comb %>%
  ggplot(aes(x = reorder(name, -age), y = age/365.25)) +
  geom_line(size = 4, color = "forestgreen") +
  geom_point(size = 2, color = "yellow1") +
  coord_flip() +
  facet_grid(gender ~ ., scales="free") +
  geom_vline(xintercept=seq(1.5, length(unique(age_slams_comb$name))-0.5, 1), 
             size = 0.5, colour = "black" , alpha = 0.1) +
  theme(
  panel.background = element_rect(fill = 'coral3'),
  panel.grid.major.x = element_line(size = 1.2),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank()
  ) +
  ggsave("./week 15/img/tennis.png", width = 8, height = 16)
