library(tidyverse)
library(cowplot)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>%
  arrange(tournament_date)   
       
t <- 
  theme(
    plot.background = element_rect(fill = "darkgreen"),
    panel.background = element_rect(fill = "coral3"),
    panel.grid.major.x = element_line(size = 1.2, color = "gray100"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 4, family = "IBM Plex Mono", color = "white")
    )
  
p1 <- age_slams_comb %>%
  filter(gender == "Female") %>%
  ggplot(aes(x = reorder(name, -age), y = age/365.25)) +
  geom_point(size = 1.5, shape = 1, color = "gray20", alpha = 0.7) +
  geom_line(color = "gray20", alpha = 0.7) +
  geom_point(size = 1, color = "yellow1") +
  coord_flip() +
  geom_vline(xintercept=seq(1.5, length(unique(age_slams_comb$name))-0.5, 1), 
             size = 0.5, colour = "black", alpha = 0.1) +
  t
       
p2 <- age_slams_comb %>%
  filter(gender == "Male") %>%
  ggplot(aes(x = reorder(name, -age), y = age/365.25)) +
  geom_point(size = 1.5, shape = 1, color = "gray20", alpha = 0.7) +
  geom_line(color = "gray20", alpha = 0.7) +
  geom_point(size = 1, color = "yellow1") +
  coord_flip() +
  geom_vline(xintercept=seq(1.5, length(unique(age_slams_comb$name))-0.5, 1), 
           size = 0.5, colour = "black", alpha = 0.1) +
  t

title <- ggdraw() +
  draw_label("Grand Slam winners, matches won by age",
                               fontfamily = "IBM Plex Mono", colour = "white", hjust = 1, size = 8) +
  theme(panel.background = element_rect(color = "white", fill = "darkgreen"))
pp <- plot_grid(p1, p2, ncol = 2)
tpp <- plot_grid(title, pp, nrow = 2,  rel_heights = c(0.1, 1, 1))
save_plot("./week 15/tennis.png", tpp, base_aspect_ratio = 1.4)
# crop 1 pixel in post-processing
