library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

ewf_appearances <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-16/ewf_appearances.csv')

ewf_cum_goals <- ewf_appearances %>% 
  group_by(team_id) %>% 
  arrange(date) %>% 
  mutate(team_names = paste(unique(str_remove(team_name, " Ladies| Women")), collapse = ", ")) %>% 
  mutate(cum_goals = cumsum(goals_for)) %>%
  mutate(istop = if_else(max(cum_goals) > 400, TRUE, FALSE)) %>% 
  mutate(team_label = if_else(istop, paste(team_names, cum_goals), team_names)) %>% 
  ungroup() 
  
ewf_cum_end <- ewf_cum_goals %>% 
  group_by(team_id) %>% 
  filter(date == max(date) & cum_goals == max(cum_goals)) %>% 
  ungroup()
  
seasons <- ewf_appearances %>% 
  group_by(season_id, division) %>% 
  summarise(
    season_start = min(date),
    season_end = max(date)
  ) %>% 
  ungroup()

seasons_overview <- seasons %>% 
  mutate(division = str_remove_all(division, "FA | \\(.+\\)")) %>% 
  group_by(division) %>% 
  mutate(
    x = case_when(
      division == "Women's Super League" ~ as.Date("2015-10-20"),
      TRUE ~ min(season_start)
    ),
    y = cur_group_id() * 20 + 640
  ) %>% 
  ungroup()

season_goals <- ewf_appearances %>% 
  group_by(team_id, season_id) %>% 
  mutate(
    team_goals = sum(goals_for),
    team_games = n(),
    team_goals_per_game = team_goals / team_games
  ) %>% 
  ungroup() %>% 
  distinct(season_id, division, team_name, team_goals, team_games, team_goals_per_game) %>% 
  left_join(seasons) %>% 
  group_by(division, season_id) %>% 
  mutate(mid_season = mean(c(season_start, season_end))) %>% 
  distinct(season_id, division, team_name, team_goals, mid_season) %>% 
  filter(team_goals == max(team_goals)) %>% 
  ungroup() %>% 
  left_join(seasons_overview, by = "season_id") %>% 
  left_join(ewf_cum_goals %>% distinct(team_name, team_names, istop)) %>% 
  filter(istop)
  
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot() +
  # Season background bars
  geom_rect(data = seasons_overview, aes(xmin = season_start, xmax = season_end, ymin = -Inf, ymax = y), fill = alpha("purple3", 0.07)) +
  # Line chart
  geom_step(data = ewf_cum_goals, aes(x = date, y = cum_goals, group = team_id, color = if_else(istop, team_names, NA), linewidth = if_else(istop, 1, 0.1))) +
  # Team end points
  geom_point(data = ewf_cum_end, aes(x = date, y = cum_goals, color = if_else(istop, team_names, NA), size = if_else(istop, 2.5, 0.2))) +
  # Top 3 teams labels
  ggrepel::geom_text_repel(data = ewf_cum_end %>% filter(istop), aes(x = date, y = cum_goals, label = team_label, color = team_names), hjust = 1, family = f1b, bg.color = "white", fontface = "bold", size = 5, seed = 99, point.padding = 10) +
  # Season segments
  geom_segment(data = seasons_overview, aes(x = season_start, xend = season_end, y = y), color = "purple4", linewidth = 2, alpha = 0.6) +
  # Season names
  geom_text(data = seasons_overview, aes(x = x - 30, y = y, label = division), hjust = 1, stat = "unique", color = "purple4", family = f1b, size = 3.5) +
  # Season goals
  shadowtext::geom_shadowtext(data = season_goals, aes(x = mid_season, y = y, label = team_goals, color = if_else(istop, team_names, NA)), bg.color = "white", family = f1b, size = 3.5, fontface = "bold") +
  scale_x_date(breaks = c(as.Date(c("2011-04-13", "2014-04-14", "2017-02-11
", "2018-09-08", "2024-05-18"))), date_labels =  "%b %Y") +
  scale_y_continuous(position = "right", limits = c(0, max(seasons_overview$y) + 10), breaks = seq(0, 600, 100)) +
  scale_color_manual(values = c("Arsenal" = "#EF0107", "Chelsea" = "#034694", "Manchester City" = "#6CABDD"), na.value = "purple4") +
  scale_linewidth_identity() +
  scale_size_identity() +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Chelsea, the team with the most goals in English women's football",
    subtitle = str_wrap("Cumulative goals of teams competing in the Women’s Super League and Women’s Championship competitions from 2011 and 2014, respectively, to 2024. The vertical bars show the different competitions and highlight the seasons when one of the top 3 teams scored the highest number of goals.", 120),
    caption = "Source: The English Women's Football (EWF) Database · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "purple4", linewidth = 0.2),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(0, 0, 15, 0), lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )
