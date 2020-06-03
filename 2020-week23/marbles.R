library(tidyverse)
library(shadowtext)
library(ggimage)
library(ggtext)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

teams <- tribble(
  ~team_name, ~team_colour,
  "Balls of Chaos", "#BC4D21",
  "Green Ducks", "#656F3C",
  "Hazers", "#636363",
  "Hornets", "#D2C642",
  "Limers", "#AEC956",
  "Mellow Yellow", "#FBE24C",
  "Midnight Wisps", "#85CEF2",
  "O'rangers", "#EE7930",
  "Raspberry Racers", "#CD3C4E",
  "Rojo Rollers", "#D6554f",
  "Savage Speeders", "#872E18",
  "Snowballs", "#9BBAE5",
  "Team Galactic", "#9162D7",
  "Team Momo", "#6A9655",
  "Team Primary", "#D9B042",
  "Thunderbolts", "#2B54A7"
)

marbles_points <- marbles %>%
  group_by(marble_name) %>% 
  mutate(points_sum = sum(points, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(marble_name, team_name, points_sum) %>% 
  left_join(teams) %>% 
  mutate(logo = here::here("2020-week23", "logos", paste0(team_name, ".png"))) %>% 
  arrange(-points_sum) %>% 
  group_by(points_sum) %>% 
  mutate(
    points_n = row_number() - 1,
    label_x = max(points_n) + 0.5,
    label = paste0(paste0("<span style='color:", team_colour, "'> ", marble_name), collapse = " ")
      ) %>% 
  ungroup() %>% 
  group_by(team_name) %>%
  mutate(
    team_n = cur_group_id(),
    marble_n = row_number(),
    from = points_sum[marble_n == 1],
    to = points_sum[marble_n == 2]
    ) %>% 
  ungroup() 

ggplot(marbles_points) +
# Line between points -----------------------------------------------------
  annotate("segment", x = 0, y = 0, xend = 0, yend = max(marbles_points$points_sum)) +
# Connect marbles of same team --------------------------------------------
  geom_curve(aes(x = 0, y = from, xend = 0, yend = to, colour = team_colour), curvature = 0.7, alpha = 0.25, size = 0.5) +
# Marble to team name -----------------------------------------------------
  geom_curve(aes(x = 0, y = to, xend = 0, yend = -5 - team_n * 2 - marble_n / 2, colour = team_colour), curvature = -1, alpha = 0.25, size = 0.5) +
# Marble points -----------------------------------------------------------
  geom_point(aes(x = points_n, y = points_sum, fill = team_colour), shape = 21, colour = "grey15", size = 3.8) +
# Labels ------------------------------------------------------------------
  geom_text(aes(x = -1, y = points_sum, label = paste0(points_sum, "")), hjust = 1, family = "IBM Plex Mono", colour = "grey70") +
  geom_richtext(aes(label_x, points_sum, label = label), hjust = 0, fill = NA, label.color = NA, family = "IBM Plex Sans Bold", size = 3.7) +
  geom_text(aes(x = 0, y = -5.75 - team_n * 2, label = team_name, colour = team_colour), hjust = 1, nudge_x = -2.5, family = "IBM Plex Sans Medium", size = 4.5) +
  geom_image(aes(x = -1, y = -5.85 - team_n * 2, image = logo), asp = 0.9, size = 0.02, by = "height") +
  annotate("text", x = 19, y = 56, label = "Marbula One:\nRacers' Championship\nstandings with total points", family = "IBM Plex Sans Medium", size = 10, colour = "grey90", hjust = 0, vjust = 1) +
  labs(
    caption = "Source: Jelle's Marble Runs | Graphic: Georgios Karamanis"
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed(clip = "off", xlim = c(-26, 60), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(colour = "grey15", fill = "grey15"),
    plot.caption = element_text(family = "IBM Plex Sans", colour = "grey60"),
    plot.margin = margin(40, 25, 40, 25)
  ) +
  ggsave(here::here("2020-week23", "plots", "temp", paste0("marbles-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 14, width = 12)

