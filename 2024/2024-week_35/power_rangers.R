library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 13, height = 7, units = "in", dpi = 320)

power_rangers_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_episodes.csv')

power_rangers_seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')

pr <- power_rangers_episodes %>% 
  janitor::clean_names() %>% 
  left_join(power_rangers_seasons %>% rename(season_imdb_rating = IMDB_rating), by = "season_title") %>% 
  mutate(calc_season_imdb = round(mean(imdb_rating), 1), .by = season_num)

pal <- MetBrewer::met.brewer("Hiroshige")

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(pr) +
  # Episode IMDB rating
  geom_tile(aes(episode_num, season_num, fill = imdb_rating)) +
  geom_text(aes(episode_num, season_num, label = imdb_rating, color = if_else(between(imdb_rating, 6, 7), "gray30", "white")), size = 2.5, family = f1b) +
  # Season IMDB rating
  geom_tile(aes(-8, season_num, fill = season_imdb_rating), width = 4, stat = "unique") +
  geom_text(aes(-8, season_num, label = season_imdb_rating, color = if_else(between(season_imdb_rating, 6, 7), "gray30", "white")), size = 3, family = f1b, stat = "unique") +
  geom_text(aes(-8, 0, label = "Season\nIMDB rating"), size = 3.5, family = f1b, vjust = 0, lineheight = 0.9, stat = "unique") +
  # Calculated season mean IMDB rating
  geom_tile(aes(-3, season_num, fill = calc_season_imdb), width = 4, stat = "unique") +
  geom_text(aes(-3, season_num, label = calc_season_imdb, color = if_else(between(calc_season_imdb, 6, 7), "gray30", "white")), size = 3, family = f1b, stat = "unique") +
  geom_text(aes(-3, 0, label = "Calculated\nseason mean*"), size = 3.5, family = f1b, stat = "unique", vjust = 0, lineheight = 0.9) +
  # Scales, etc.
  scale_x_continuous(breaks = c(1, seq(10, 60, 10))) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 27)) +
  scale_fill_stepsn(colors = pal) +
  scale_color_identity() +
  coord_fixed(clip = "off", expand = FALSE) +
  labs(
    title = "Power Rangers IMDB ratings",
    # subtitle = "Subtitle",
    caption = "*Episode ratings were used to calculate the season mean\nfor comparison with the IMDb season rating\n\nSource: kaggle · Graphic: Georgios Karamanis",
    fill = "IMDB rating",
    x = "Episode",
    y = "Season"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.8, 0.3),
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(1, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), size = 10, lineheight = 1),
    plot.margin = margin(10, 10, 10, 10)
  )
