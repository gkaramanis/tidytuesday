library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.5, height = 10, units = "in", dpi = 320)

holiday_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episodes.csv')

holiday_episode_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episode_genres.csv')

holiday_episodes_sel <- holiday_episodes %>% 
  filter(str_detect(genres, "Comedy"))

outliers <- holiday_episodes_sel %>% 
  mutate(rating_diff = average_rating - parent_average_rating) %>% 
  filter(abs(rating_diff) >= 3.5)
  
f1 <- "Outfit"
f2 <- "Bricolage Grotesque"

ggplot(holiday_episodes_sel) +
  geom_abline(slope = 1, color = "grey50", linetype = "dashed") +
  ggpointdensity::geom_pointdensity(aes(parent_average_rating, average_rating), size = 2.5, alpha = 0.9) +
  ggrepel::geom_text_repel(data = outliers, aes(parent_average_rating, average_rating, label = paste0(original_title, " (", year, ")", "\n", parent_original_title)), size = 3.3, min.segment.length = 0, family = f2, seed = 99, bg.color = "white", bg.r = 0.12, lineheight = 0.9, segment.size = 0.2, point.padding = 0.3, force = 2, force_pull = 0, color = "purple4") +
  scale_x_continuous(minor_breaks = seq(2, 10, 0.5), limits = c(2, 10)) +
  scale_y_continuous(minor_breaks = seq(2, 10, 0.5)) +
  MetBrewer::scale_color_met_c("Hokusai1", direction = -1) +
  coord_fixed() +
  labs(
    title = "How well do holiday TV episodes of comedies perform?",
    subtitle = str_wrap("Weighted average IMDb ratings of holiday episodes compared to the series overall. Episodes with the most significant deviation from the series rating are labeled.", 90),
    caption = "Source: IMDb · Graphic: Georgios Karamanis",
    x = "Average rating of series",
    y = "Average rating of holiday episodes"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F8FFF3", color = NA),
    axis.title = element_text(face = "bold", color = "darkgreen", size = 12),
    axis.text.x = element_text(margin = margin(0, 0, 8, 0), size = 11, color = "darkgreen"),
    axis.text.y = element_text(margin = margin(0, 0, 0, 8), size = 11, color = "darkgreen"),
    panel.grid = element_line(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(15, 10, 10, 10)
  )
  
