library(tidyverse)
library(ggimage)
library(here)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

winloss <- wwc_outcomes %>%
  # add_count(team, name = "team_t") %>% 
  # mutate(team = fct_reorder(team, team_t, .desc = T)) %>% 
  group_by(team) %>%
  mutate(
    game_n = row_number(),
    win_i = -2 + as.integer(factor(win_status))
   )

ggplot(winloss) +
  # grey background bars
  geom_tile(aes(x = 25.5, y = 0), height = 3, 
            width = 50, fill = "grey97") +
  geom_tile(aes(x = 25.5, y = 0), height = 1,
            width = 50, fill = "grey93") +
  # win/draw/loss boxes
  geom_tile(aes(x = game_n, y = win_i, alpha = win_i,
                fill = as.factor(win_i)), color = "white") +
  # country codes and flags
  geom_text(aes(label = team, x = -1, y = 0),
    hjust = 1, size = 3, check_overlap = TRUE,
    family = "IBM Plex Mono Bold") +
  geom_image(x = -10, y = 0, asp = 20, size = 0.04,
             aes(image = here("week-28", "flags", paste0(team, ".png")))) +
  # Scales
  scale_fill_manual(values = c("#8c5358", "#465675", "#27b376"),
    labels = c("loss", "draw", "win")) +
  coord_fixed(xlim = c(-10, 50)) +
  scale_x_continuous(breaks = c(10, 30, 50)) +
  scale_alpha_continuous(range = c(1, 1), guide = F) +
  # Title, subtitle and caption
  labs(
    title = "Wins, draws and losses for the teams that have taken part in\nthe Women's World Cup from 1991 to 2019",
    subtitle = "USA have the most total wins (42), followed by Germany (31) and Norway (23). USA have the\ntwo longest winning streaks (12 and 11) and Norway the third longest (10). Germany hold the\nlongest undefeated streak (15), USA the second and third one (14 and 11).",
    caption = "Source: data.world | Graphic: Georgios Karamanis",
    x = "Number of matches played in WWC"
  ) +
  facet_wrap(~ team, ncol = 2) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = "top",
    legend.key.size = unit(0.35, "line"),
    legend.text = element_text(color = "grey60", size = 6,
                               family = "IBM Plex Mono"),
    legend.title = element_blank(),
    plot.margin = margin(20, 60, 20, 60),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "grey70", size = 7,
                               family = "IBM Plex Mono"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color = "grey60", size = 7,
                               family = "IBM Plex Mono"),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11, family = "IBM Plex Serif Medium"),
    plot.subtitle = element_text(size = 8, family = "IBM Plex Sans",
                                 margin = margin(0, 0, 25, 0)),
    plot.caption = element_text(size = 7, color = "grey60",
                                family = "IBM Plex Mono",
                                margin = margin(25, 0, 0, 0)),
    strip.text = element_blank()
  ) +
  # http://www.storytellingwithdata.com/blog/2019/6/27/power-pairing-color-words
  ggsave(here("week-28", "wwc.png"),
         width = 6, height = 6, dpi = 300)
