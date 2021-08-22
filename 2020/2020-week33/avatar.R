library(tidyverse)
library(fuzzyjoin)
library(colorspace)
library(futurevisions)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

shots <- read_csv(here::here("2020-week33", "data", "shots.csv"))

f <- c("aerial shot", "back view", "frontal view", "ground level shot", "side view", "panning", "zoom", "over the shoulder shot", "overhead shot", "point of view shot", "close-up shot", "wide shot", "cut", "fade")

tf <- c("camera angle", "camera movement", "shot size", "point of view", "editing")
  
scene_shots_joined <- avatar %>%
  select(id:chapter_num) %>% 
  left_join(scene_description) %>% 
  regex_left_join(shots, by=c("scene_description" = "string"))

scene_shots <- scene_shots_joined %>% 
  distinct(id, shot, .keep_all = TRUE) %>% 
  filter(!is.na(shot)) %>% 
  add_count(book_num, chapter_num, shot) %>% 
  group_by(shot) %>% 
  mutate(
    median_n = median(n),
    n = n - median_n,
    median_n = 0
  ) %>% 
  ungroup() %>% 
  distinct(book_num, book, type, shot, n, median_n) %>% 
  mutate(
    shot = fct_relevel(shot, rev(f)),
    type = fct_relevel(type, tf))

pal <- c(futurevisions("pegasi")[3:6], futurevisions("pegasi")[2])

ggplot(scene_shots) +
  geom_vline(xintercept = 0, colour = darken("#C5C5C0", 0.1)) +
  geom_linerange(aes(xmin = n, xmax = median_n, y = shot, group = shot, colour = type), size = 0.8, key_glyph = draw_key_blank) +
  geom_point(aes(n, shot, fill = type, colour = type), size = 4.5, shape = 21, stroke = 1) +
  scale_fill_manual(values = pal, guide = guide_legend(title = NULL)) +
  scale_colour_manual(values = darken(pal, 0.2), guide = guide_legend(title = NULL)) +
  facet_wrap(vars(fct_reorder(book, book_num))) +
  labs(
    title = "Avatar: The Last Airbender",
    subtitle = str_wrap("Number of camera shots and transitions* in every episode, shown as deviation from the median value for the whole series", 65),
    caption = "*As mentioned in the transcripts\nSource: appa R package | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Graphik Compact") +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 10, 0),
    legend.text = element_text(size = 13),
    plot.background = element_rect(fill = "#EBE5D5", colour = NA),
    strip.text = element_text(size = 20, margin = margin(0, 0, 20, 0), hjust = 0.3, family = "Canela Deck Bold"),
    panel.spacing.x = unit(4, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#C5C5C0", size = 0.3),
    panel.grid.minor.x = element_line(colour = "#C5C5C0", size = 0.2),
    axis.text = element_text(size = 14, colour = "#21415C"),
    axis.title = element_blank(),
    plot.title = element_text(size = 30, hjust = 0.5, family = "Canela Deck Bold"),
    plot.subtitle = element_text(size = 20, hjust = 0.5, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 55, 20, 40)
  )

  # ggsave(paste0("temp/avatar-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), dpi = 320, width = 13, height = 11)

