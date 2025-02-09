library(tidyverse)
library(ggrepel)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')

f1 <- "Golos UI"
f2 <- "Inclusive Sans"

ggplot(simpsons_episodes, aes(imdb_rating, imdb_votes, label = paste0("S", season, "E", number_in_season, "\n", title))) +
  geom_smooth(color = "#FED90F", fill = alpha("#FED90F", 0.2)) +
  geom_point(aes(size = us_viewers_in_millions), alpha = 0.6, stroke = 0.7, shape = 21, fill = "#82B461", color = "#3362AD") +
  geom_text_repel(data = . %>% filter(imdb_rating > 7.5 | imdb_rating < 6), size = 4, hjust = 0, seed = 9, family = f2, lineheight = 0.95, min.segment.length = 0, segment.color = "#3362AD", max.overlaps = 3, bg.color = "white", point.padding = 0.5) +
  scale_size_area(max_size = 5, name = "US viewers (millions)", limits = c(2, 15), breaks = seq(5, 14.5, 2.5)) +
  MetBrewer::scale_color_met_c("Homer2") +
  guides(size = guide_legend(nrow = 1)) +
  labs(
    title = "When Lady Gaga met Springfield (and fans weren't happy)",
    subtitle = str_wrap("The most liked and disliked Simpsons episodes tend to attract more votes on IMDB, showing a U-shaped relationship between ratings and votes. The Lady Gaga episode (S23E22) stands out with the most votes and the lowest rating for episodes in the 2010-2016 period.", 98),
    caption = "Source: The Simpsons Dataset (Kaggle) · Graphic: Georgios Karamanis",
    x = "IMDB Rating",
    y = "Number of votes"
    ) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(color = "#D6503E"),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_text(color = "#D6503E"),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(hjust = 0)
  )
