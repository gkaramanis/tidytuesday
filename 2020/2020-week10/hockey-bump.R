library(tidyverse)
library(ggbump)
library(ggrepel)
library(here)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

goals_discs <- game_goals %>% 
  select(player, goals) %>%
  group_by(player) %>%
  mutate(
    game_n = 1:n(), # number the games for every player
    total_games = max(game_n),
    total_goals = sum(goals),
    cumsum_goals = cumsum(goals), # calculate running total
    r500 = detect_index(cumsum_goals, ~. > 500), # find the first game that the total goals was over 500 
    r300 = detect_index(cumsum_goals, ~. > 300), # find the first game that the total goals was over 300
  ) %>%
  ungroup() %>%
  filter(total_goals > 500) %>% # filter players with more than 500 total goals
  distinct(player, total_goals, total_games, r500, r300) %>% 
  pivot_longer(cols = total_games:r300, names_to = "r", values_to = "r_v") %>% 
  mutate(
    r_v = na_if(r_v, 0),
    line_colour = case_when(
      player == "Jaromir Jagr" ~ "#DB3A2F",
      player == "Mike Gartner" ~ "#DC942F",
      player == "Wayne Gretzky" ~ "#275D8E",
      player == "Brett Hull" ~ "#698E7C",
      player == "Alex Ovechkin" ~ "#902A57",
      TRUE ~ "grey80"
    )
  )

ggplot() +
  # Plot not highlighted players
  geom_bump(data = subset(goals_discs, line_colour == "grey80"),
            aes(x = r, y = r_v, group = player,
                colour = line_colour, size = total_goals, smooth = 7)) +
  # Plot highlighted players
  geom_bump(data = subset(goals_discs, line_colour != "grey80"),
            aes(x = r, y = r_v, group = player,
                colour = line_colour, size = total_goals, smooth = 7)) +
  # Labels for both highlighted and not highlighted players
  geom_text_repel(data = subset(goals_discs, r == "total_games"),
                  aes(x = r, y = r_v, label = paste0(player, ": ", total_goals),
                      colour = line_colour, size = total_goals * 1.5),
                  nudge_x = 0.05, hjust = 0, direction = "y",
                  box.padding = 1, point.padding = 0.5,
                  family = "IBM Plex Sans") +
  # Draw axis arrow
  geom_segment(aes(x = Inf, xend = Inf , y = 350, yend = 1880), size = 0.7,
               arrow = arrow(length = unit(0.6, "cm"))) +
  scale_x_discrete(expand = expansion(add = c(0.05, 0.6)), labels = c("Number of games\nto reach 300 goals", "to reach 500 goals", "to reach total goals"), position = "top") +
  # Title and caption
  annotate("text", x = 1.05, y = 1700, label = str_wrap("Number of games played to reach 300, 500 and total career goals, for players with 500 or more total goals", 40), hjust = 0, family = "IBM Plex Sans Bold", size = 8) +
  annotate("text", x = 1.05, y = 1850, label = "Source:  HockeyReference.com | Graphic: Georgios Karamanis", hjust = 0, family = "IBM Plex Sans", size = 4) +
  scale_y_reverse(breaks = seq(0, 1800, 200), position = "right", "Games played\n") +
  scale_size_continuous(range = c(1, 7)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "JetBrains Mono", base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = "#343854"),
    axis.text.x = element_text(hjust = 0.1, colour = "#343854"),
    axis.text.y = element_text(hjust = 1, colour = "#343854"),
    plot.margin = margin(40, 30, 40, 40)
  ) 

ggsave(here::here("2020-week10", "plots", "hockey-bump.png"), dpi = 320, width = 16, height = 11)

