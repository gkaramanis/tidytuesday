library(tidyverse)
library(here)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

winloss <- wwc_outcomes %>%
  group_by(team) %>%
  mutate(
    game_n = row_number(),
    win_i = -2 + as.integer(factor(win_status))
   )
  
ggplot(winloss) +
  geom_tile(aes(x = 26, y = 0), height = 3, 
            width = 52, fill = "grey90") +
  geom_tile(aes(x = game_n, y = win_i), color = "white") +
  geom_text(aes(label = team, x = -1, y = 0),
    hjust = 1, size = 3, check_overlap = TRUE,
    family = "IBM Plex Mono") +
  coord_fixed(xlim = c(-2, 50)) +
  facet_wrap(~ team, ncol = 2) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    strip.text = element_blank()
  ) +
  ggsave(here("week-28", "wwc.png"))
  
  
  
  
  
  
