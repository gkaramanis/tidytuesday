library(tidyverse)
library(ggbump)

ggplot(marbles_points) +
  geom_sigmoid(aes(x = 0, y = points_sum, xend = 10, yend = team_n * 3.75 - 2, group = marble_name, colour = team_colour), size = 1.5, alpha = 0.9) +
  geom_richtext(aes(x = -0.1, y = points_sum, label = label, colour = team_colour), hjust = 1, fill = NA, label.color = NA) +
  geom_richtext(aes(x = 10.1, y = points_sum, label = team_name, colour = team_colour), hjust = 0, fill = NA, label.color = NA) +
  scale_color_identity() +
  theme_void() 

ggsave(here::here("2020-week23", "plots", "temp", paste0("marbles-sigmoid-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 12)

