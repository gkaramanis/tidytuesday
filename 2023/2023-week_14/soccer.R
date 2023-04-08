library(tidyverse)
library(gghighlight)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv') %>% 
  janitor::clean_names() %>% 
  mutate(id = row_number(), .before = 1)

# Team abbreviations
teams <- tribble(
  ~team, ~abbr,
  "Arsenal", "ARS",
  "Aston Villa", "AVL",
  "Brentford", "BRE",
  "Brighton", "BHA",
  "Burnley", "BUR",
  "Chelsea", "CHE",
  "Crystal Palace", "CRY",
  "Everton", "EVE",
  "Leeds", "LEE",
  "Leicester", "LEI",
  "Liverpool", "LIV",
  "Man City", "MCI",
  "Man United", "MUN",
  "Newcastle", "NEW",
  "Norwich", "NOR",
  "Southampton", "SOU",
  "Tottenham", "TOT",
  "Watford", "WAT",
  "West Ham", "WHU",
  "Wolves", "WOL"
)

# All match results
results <- soccer %>% 
  mutate(
    date = dmy(date),
    hp = case_when(
      ftr == "H" ~ 3,
      ftr == "D" ~ 1,
      TRUE ~ 0
    ),
    ap = case_when(
      ftr == "A" ~ 3,
      ftr == "D" ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(id, date, home_team, away_team, ftr, hp, ap) %>% 
  pivot_longer(hp:ap, names_to = "home_away", values_to = "points") %>% 
  pivot_longer(home_team:away_team, values_to = "team") %>% 
  filter((home_away == "hp" & name == "home_team") | (home_away == "ap" & name == "away_team")) 

# Standings (cumulative points for each team)
standings <- results %>%
  arrange(date) %>% 
  group_by(team) %>% 
  mutate(
    total_points = cumsum(points),
    max_points = max(total_points)
    ) %>% 
  ungroup() %>% 
  mutate(y = as.numeric(date)) %>% 
  left_join(teams)

# Matches between Manchester City and Liverpool
vs <- soccer %>% 
  filter((home_team == "Man City" & away_team == "Liverpool") | (away_team == "Man City" & home_team == "Liverpool")) %>% 
  mutate(
    date = dmy(date),
    y = as.numeric(date)
    ) %>% 
  left_join(standings)

# Biggest point difference between Manchester City and Liverpool
max_diff <- standings %>% 
  filter(team == "Man City" | team == "Liverpool") %>% 
  select(date, team, total_points, y) %>% 
  pivot_wider(names_from = team, values_from = total_points) %>% 
  fill(`Liverpool`, .direction = "down") %>% 
  fill(`Man City`, .direction = "down") %>% 
  mutate(
    points_diff = abs(`Liverpool` - `Man City`),
    max_diff = max(points_diff, na.rm = TRUE),
    x = `Liverpool` + (`Man City` - `Liverpool`) / 2
  ) %>% 
  filter(max_diff == points_diff)

f1 <- "Outfit"
f2 <- "Iosevka Fixed"
  
ggplot(standings) +
  # Point lines
  # This is for the outline
  ggborderline::geom_borderstep(aes(x = total_points, y = y, group = team, color = team), linewidth = 0.7, direction = "vh") +
  # This is only for the gghighlight labels
  geom_line(aes(x = total_points, y = y, group = team, color = team), linewidth = 0) +
  # Final points
  geom_point(aes(x = max_points, y = max(y), color = team), stat = "unique") +
  # Labels for teams
  ggrepel::geom_text_repel(data = . %>% filter(between(max_points, 55, 90) | max_points <= 35), aes(max_points, max(y), label = paste0(abbr, "·", max_points), color = team), stat = "unique", family = f2, nudge_y = -10, vjust = 0, size = 3, direction = "x", segment.color = NA) +
  ggrepel::geom_text_repel(data = . %>% filter(max_points > 90), aes(max_points, max(y), label = paste0(abbr, "·", max_points), color = team), stat = "unique", family = f2, nudge_y = -10, vjust = 0, size = 3, direction = "x", segment.color = NA) +
  # Highlight Manchester City and Liverpool
  gghighlight(max(total_points) > 90, unhighlighted_params = list(color = "grey75"), label_params = list(direction = "y", vjust = 1, nudge_y = 50, family = f1, fontface = "bold", bg.color = "grey98", bg.r = 0.3, box.padding = 0.5, point.padding = 1, size = 5), line_label_type = "ggrepel_text") +
  # Matches between the two top teams
  geom_point(data = vs, aes(total_points, y)) +
  # Annotate the matches between the two top teams
  # The two annotations are similar but the first one has thicker lines, in order to make the second more readable
  ggforce::geom_mark_circle(data = vs, aes(x = total_points, y = y, group = id, label = paste(format(date, "%b %d"), "\n", home_team, fthg, "-", ftag, away_team)), label.buffer = unit(15, "line"), label.fontsize = 10, label.family = f1, con.colour = "grey98", con.size = 1.4, color = "orange2", label.fontface = "plain") +
  ggforce::geom_mark_circle(data = vs, aes(x = total_points, y = y, group = id, label = paste(format(date, "%b %d"), "\n", home_team, fthg, "-", ftag, away_team)), label.buffer = unit(15, "line"), label.fontsize = 10, label.family = f1, con.colour = "orange2", con.size = 0.5, color = "orange2", label.fontface = "plain", fill = "orange") +
  # Biggest point gap
  geom_segment(data = max_diff, aes(x = `Liverpool`, xend = `Man City`, y = y, yend = y), color = "orange2", size = 0.3) +
  ggforce::geom_mark_rect(data = max_diff, aes(x = x, y = y, label = paste0("The biggest gap between\n Manchester City and Liverpool\nwas ", max_diff, " points on ", format(date, "%b %d"))), label.fontsize = 10, label.family = f1, con.colour = "orange2", con.size = 0.5, color = "orange2", label.fontface = "plain", expand = 0, radius = 0.0001, label.buffer = unit(8, "line")) +
  # Scales, etc
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_reverse(labels = function(x) format(as_date(x), "%b %d, %Y")) +
  scale_color_manual(values = c("#C8102E", "#1C2C5B")) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Race to the finish line",
    subtitle = "Manchester City vs Liverpool in the 2021/22 Premier League title race",
    caption = "Source: Evan Gower · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey98", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_text(family = f2),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 24, family = "Publico Headline", face = "bold", color = "#1C2C5B"),
    plot.margin = margin(20, 20, 10, 20),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0, color = "#1C2C5B"),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), color = "#C8102E", face = "bold", size = 15)
  )
  
