library(tidyverse)
library(here)
library(colorspace)

# songs = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv')

x_songs <- read_rds(here("week-52", "data", "christmas-songs.rds")) %>% 
  select(title = title_x, everything()) %>% 
  mutate(
    title = str_to_title(title),
    year =  as.numeric(str_sub(week_id, -4))
    ) %>% 
  select(title, year, week_position) %>% 
  filter(week_position < 25)


# https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
x_pal <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#42d4f4", "#f032e6", "#fabebe", "#469990", "#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#000075", "#a9a9a9")

ggplot(x_songs, aes(year, week_position)) +
  geom_segment(aes(xend = year, yend = 1), color = "#aab5c1", size = 0.3) +
  geom_tile(aes(y = week_position-0.19, height = 0.2, width = 0.15), size = 0.25, fill = "yellow", color = "grey20") +
  geom_point(aes(fill = title, color = title), size = 5, shape = 21, stroke = 0.25) +
  geom_point(aes(year + 0.17, week_position - 0.1), color = "white", size = 0.9, alpha = 0.4) +
  # annotate("text", x = seq(1965, 2015, 10), y = 3, label = paste0("'", str_sub(seq(1960, 2010, 10), -2), "s"), size = 19, alpha = 0.05, family = "IBM Plex Sans Bold") +
  geom_segment(aes(x = 1999, xend = 1999, y = 1, yend = 7), color = "#aab5c1", size = 0.3) +
  scale_x_continuous(breaks = c(seq(1960, 2020, 10))) +
  scale_y_reverse(limits = c(25, 1), expand = c(0, 0), breaks = c(1:10, 15, 20, 25), sec.axis = dup_axis()) +
  scale_fill_manual(values = x_pal, guide = guide_legend(rows = 4, byrow = TRUE)) +
  scale_color_manual(values = darken(x_pal, 0.8)) +
  labs(
    title = "Christmas songs that made it to the Billboard Hot 100's Top 25",
    subtitle = str_wrap("From the 1959-60 to the 2018-19 holiday season. Christmas songs taken from Wikipedia's 'List of popular Christmas singles in the United States' and matched with songs in the dataset 'Billboard Hot weekly charts' by Sean Miller on data.world.", width = 125),
    caption = "Data: Wikipedia, data.world | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(
    # legend.position = "bottom",
    legend.position = c(0.65, 0.6),
    legend.box.background = element_rect(fill = "grey98", color = "#aab5c1", size = 0.3),
    legend.text = element_text(size = 9, family = "IBM Plex Serif"),
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "grey93", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 19),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(
    here::here("week-52", "plots", "temp", paste0("christmas-balls-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 10
  )

  
