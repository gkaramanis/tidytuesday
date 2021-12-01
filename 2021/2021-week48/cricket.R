library(tidyverse)
library(camcorder)
library(colorspace)
library(ggtext)

gg_record(dir = "temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

cricket_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')


results <- cricket_matches %>% 
  select(match_id, team1, team2, winner, match_date) %>% 
  pivot_longer(team1:team2, values_to = "team") %>% 
  mutate(isWin = team == winner)

a = 2

tree <- results %>% 
  group_by(team) %>% 
  mutate(
    angle = if_else(isWin, -a * pi/180, a * pi/180),
    a = cumsum(angle + lag(a, default = 0)) + pi / 2,
    x = 0,
    y = 0
  ) %>% 
  mutate(
    x = cumsum(lag(x, default = 0) + cos(lag(a, default = 0))) -1,
    y = cumsum(lag(y, default = 0) + sin(lag(a, default = 0))),
    wins = sum(isWin),
    losses = sum(!isWin),
    win_pct = round(wins / (wins + losses) * 100)
  ) %>% 
  ungroup() %>% 
  filter(wins > 1)

team_labels <- tree %>% 
  select(match_id, team, x, y, wins, losses, win_pct) %>% 
  group_by(team) %>% 
  slice_max(match_id, n = 1) %>% 
  ungroup() %>% 
  mutate(
    h = ifelse(x > 0, 0, 1),
    nx = ifelse(x > 0, 1, -1),
    label = paste0("**", team, "**", "<br><span style='font-size:10pt'>", wins + losses, " matches<br>", win_pct, "% wins</span>")
    )

f1 = "Proxima Nova"
f2 = "Produkt"

ggplot(tree) +
  geom_path(aes(x = x, y = y, group = team, color = isWin), size = 0.8, linejoin = "mitre", lineend = "butt", alpha = 0.9) +
  # Labels for countries with more wins
  geom_richtext(data = subset(team_labels, nx == 1), aes(x = x, y = y, label = label, hjust = h), nudge_x = 8, family = f2, lineheight = 0.9, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), vjust = 0.8, size = 4.5) +
  # Labels for countries with more losses
  geom_richtext(data = subset(team_labels, nx == -1), aes(x = x, y = y, label = label, hjust = h), nudge_x = -8, family = f2, lineheight = 0.9, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), vjust = 0.8, size = 4.5) +
  # Wins/Losses annotation
  annotate("richtext", x = 5, y = -3, label = "more <span style='color:darkolivegreen3'>**wins**</span>", hjust = 0, family = f1, size = 4, color = "grey40", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 3.6), "pt")) +
  annotate("richtext", x = -5, y = -3, label = "more <span style='color:mediumorchid3'>**losses**</span>", hjust = 1, family = f1, size = 3.5, color = "grey40", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  # Title, subtitle and caption
  annotate("richtext", x = -220, y = -85,
           label = "**ICC Men's Cricket World Cup**<br>
           <span style='font-size:18pt'>1996-2005, teams with more than 1 win</span><br>
           <span style='font-size:10pt'>Source: ESPN Cricinfo · Graphic: Georgios Karamanis</span>
           ",
           family = f2, size = 10, hjust = 0, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), lineheight = 0.9) +
  scale_color_manual(values = c("mediumorchid3", "darkolivegreen3")) +
  xlim(-220, 220) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 40, 20)
  ) 

