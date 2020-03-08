library(tidyverse)
library(here)
library(cowplot)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

goals_discs <- game_goals %>% 
  select(player, goals) %>%
  group_by(player) %>%
  mutate(
    game_n = 1:n(),
    total_games = max(game_n),
    total_goals = sum(goals),
    cumsum_goals = cumsum(goals),
    r700 = detect_index(cumsum_goals, ~. > 700),
    r500 = detect_index(cumsum_goals, ~. > 500),
    r300 = detect_index(cumsum_goals, ~. > 300)
  ) %>%
  ungroup() %>%
  filter(total_goals > 300) %>% 
  distinct(player, total_goals, total_games, r700, r500, r300) %>% 
  pivot_longer(cols = r700:r300, names_to = "r", values_to = "r_v") %>% 
  mutate(
    player = fct_reorder(player, desc(total_goals)),
    r_v = na_if(r_v, 0)
  )

p <- ggplot(goals_discs) +
  geom_point(aes(0, 0, size = total_games)) +
  geom_point(aes(0, 0, size = r_v, colour = r)) +
  scale_radius(range = c(0, 30)) +
  scale_color_manual(values = c("#275D8E", "#EAB33A", "#DB3A2F")) +
  facet_wrap(vars(player), ncol = 5) +
  theme_void(base_family = "JetBrains Mono Bold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

l <- ggplot(data = subset(goals_discs, player == "Jaromir Jagr")) +
  geom_point(aes(0, 0, size = total_games)) +
  geom_point(aes(0, 0, size = r_v, colour = r)) +
  scale_radius(range = c(0, 30)) +
  scale_color_manual(values = c("#275D8E", "#EAB33A", "#DB3A2F")) +
  theme_void(base_family = "JetBrains Mono Bold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

plot_grid(l, p, rel_widths = c(1, 3)) 
ggsave(here::here("2020-week10", "plots", "hockey-discs.png"), dpi = 320, width = 12, height = 9)
