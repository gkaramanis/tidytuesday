library(tidyverse)
library(gggibbous)
library(colorspace)
library(lubridate)

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

teams <- tribble(
  ~team_name, ~team_colour,
  "Balls of Chaos", "#BC4D21",
  "Green Ducks", "#656F3C",
  "Hazers", "#636363",
  "Hornets", "#D2C642",
  "Limers", "#AEC956",
  "Mellow Yellow", "#fbd75e",
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
) %>% 
  mutate(
    team_n = row_number(),
    team_x = rep(c(1, 2), each = 8),
    team_y = rep(1:8, length.out = 16) + rep(c(-0.2, 0.2), each = 8)
    )

marbles_times <- marbles %>% 
  filter(str_detect(race, "S1R") & !is.na(time_s)) %>% 
  select(race, marble_name, time_s, points, team_name) %>% 
	arrange(race, desc(time_s)) %>% 
  mutate(
    minutes = time_s %/% 60,
    seconds = as.integer(time_s %% 60),
    milliseconds =  as.integer((time_s - minutes * 60 - seconds) * 100),
    time_msm = paste0(minutes, ":", seconds, ".", milliseconds)
  ) %>% 
  group_by(race) %>% 
  mutate(
    race_n = cur_group_id(),
    time_pct = -(time_s / min(time_s, na.rm = TRUE)) + 2,
		desat = if_else(row_number() == n(), 0, 1)
  ) %>% 
  ungroup() %>% 
  left_join(teams)

winners <- marbles_times %>% 
  group_by(race) %>% 
  slice_min(time_s)

top32 <- marbles_times %>% 
  group_by(marble_name) %>%
  mutate(points_total = sum(points)) %>% 
  ungroup() %>% 
  distinct(marble_name, points_total, team_colour) %>% 
  # slice_max(points_total, n = 20) %>% 
  arrange(desc(points_total)) %>% 
  mutate(
    position = row_number(),
    top32_y = position / 4 + 0.5
    )

ggplot(marbles_times) +
  # Place indicator for all marbles
  geom_segment(aes(x = time_pct, y = -race_n - 0.23, xend = time_pct, yend = -race_n), colour = "grey60") +
  # Line for winner
  geom_segment(data = winners, aes(x = time_pct, y = -race_n, xend = time_pct, yend = -race_n - 0.45), colour = "grey20") +
  geom_segment(data = winners, aes(x = time_pct, y = -race_n - 0.45, xend = time_pct - 0.0007, yend = -race_n - 0.45), colour = "grey20") +
  # Track line
  geom_segment(aes(x = 0.942, y = -race_n, xend = 1, yend = -race_n), colour = "grey20", size = 13, lineend = "round") +
	# Marbles
  geom_moon(aes(x = time_pct, y = -race_n, fill = desaturate(team_colour, desat)), ratio = 1, size = 11.5, right = TRUE, colour = NA) +
  # Marbles shadow
  geom_moon(aes(x = time_pct, y = -race_n, fill = desaturate(darken(team_colour), desat)), ratio = 0.25, size = 11.5, right = FALSE, colour = NA) +
  # Winners' name and time
  geom_text(data = winners, aes(x = time_pct - 0.001, y = -race_n - 0.45, label = paste0(marble_name, " ", time_msm)), family = "IBM Plex Sans Condensed Bold", hjust = 1) +
  # Race number
  geom_text(aes(x = 0.942, y = -race_n - 0.35, label = paste0("Race ", race_n)), family = "IBM Plex Sans Condensed Light", hjust = 0, colour = "grey50") +
  # Legend with teams
  geom_moon(data = teams, aes(x = 0.925 + 0.0053 * team_x, y = -team_y, fill = team_colour), ratio = 1, right = TRUE, colour = NA) +
  geom_moon(data = teams, aes(x = 0.925 + 0.0053 * team_x, y = -team_y, fill = team_colour), ratio = 0.25, right = FALSE, colour = NA) +
  geom_text(data = teams, aes(x = 0.925 + 0.0053 * team_x, y = -team_y - 0.35, label = team_name, colour = team_colour), family = "IBM Plex Sans Condensed Medium") +
  # Finish line
  annotate("tile", x = 1.0022 + 0.001 * rep(0:1, length = 38), y = -seq(0.8, 8.2, by = 0.2), height = 0.2, width = 0.001) +
  # Standings
  geom_text(data = top32, aes(x = 1.007, y = -top32_y, label = paste0(position, ". ", marble_name, " ", points_total, "p"), hjust = 0, colour = team_colour), family = "IBM Plex Sans Condensed Medium") +
  # Scales and theme
  labs(
    title = "Marbula One 2020",
    subtitle = "Middle: Results of the 8 races with name and time of the winning (highlighted) marble.\nLeft: Team names and colours. Right: Final standings with total points.",
    caption = "Source: Jelle's Marble Runs | Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(clip = "off") +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey99", colour = "grey99"),
    plot.margin = margin(20, 80, 20, 20),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 25, hjust = 0.5, margin = margin(20, 0, 0, 0)),
    plot.subtitle = element_text(family = "IBM Plex Serif Medium", size = 16, hjust = 0.5, margin = margin(20, 0, 0, 0)),
    plot.caption = element_text(family = "IBM Plex Sans", hjust = 0.5)
  ) +
  ggsave(here::here("2020-week23", "plots", "temp", paste0("marbles-race-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 15)

  
