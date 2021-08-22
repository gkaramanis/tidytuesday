library(tidyverse)
library(here)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

goals_timeline <- game_goals %>% 
  select(player, goals) %>%
  group_by(player) %>%
  mutate(
    game_n = 1:n(),
    total_goals = sum(goals),
    cumsum_goals = cumsum(goals),
    bar_colour = case_when(
      cumsum_goals > 700 ~ "#902A57",
      cumsum_goals > 500 ~ "#F18B00",
      cumsum_goals > 300 ~ "#275D8E",
      TRUE ~ "grey50"
    )
  ) %>%
  ungroup() %>%
  mutate(
    player = fct_reorder(player, total_goals)
    ) %>% 
  filter(total_goals > 500)

player_label <- goals_timeline %>%
  group_by(player) %>% 
  distinct(player, max_game = max(game_n), total_goals) %>% 
  ungroup()

ggplot(goals_timeline) +
  geom_tile(aes(x = game_n, y = player,
                width = 1, height = 0 + goals/5, fill = bar_colour), colour = NA) +
  geom_text(data = player_label, aes(x = max_game, y = player, label = paste0(player, " ", total_goals)), check_overlap = TRUE, size = 3, hjust = 0, nudge_x = 30, family = "IBM Plex Sans") +
  coord_cartesian(clip = "off", xlim = c(0, 1800)) +
  scale_fill_identity() +
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "#F7EBD3", colour = NA),
    legend.position = "top",
    legend.key.width = unit(2, "mm"),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 80, 20, 20)
  ) 

ggsave(here::here("2020-week10", "plots", "hockey-goals.png"), dpi = 320, width = 16, height = 8)
  
