library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11.5, height = 8, units = "in", dpi = 320)

episode_metrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')

imdb_wiki <- read_csv("https://raw.githubusercontent.com/poncest/bobsburgersR/refs/heads/main/data-raw/IMDb_Wikipedia_Bobs_Burgers_Data_Clean.csv")

metrics_imdb_wiki <- episode_metrics %>% 
  left_join(imdb_wiki)

f1 <- "Graphik"
f1b <- "Graphik Compact"

# avg_length: The average number of characters (technically codepoints) per line of dialogue
# unique_words:	The number of unique lowercase words in this episode

ggplot(metrics_imdb_wiki, aes(avg_length, unique_words)) +
  #S13 and S14
  geom_mark_hull(aes(fill = factor(season), filter = avg_length < 30, label = glue::glue("Season {season}")), concavity = 10, show.legend = FALSE, label.family = f1b, con.size = 0.35, alpha = 0.2) +
  # Outliers
  geom_mark_circle(aes(filter = unique_words == max(unique_words), label = glue::glue("S{season}E{episode}\n{imdb_title}")), label.family = f1b, label.fontface = "plain", label.fontsize = 10, con.size = 0.35) +
  geom_mark_circle(aes(filter = avg_length == max(avg_length), label = glue::glue("S{season}E{episode}\n{imdb_title}")), label.family = f1b, label.fontface = "plain", label.fontsize = 10, con.size = 0.35) +
  # Points
  geom_point(aes(size = wikipedia_viewers, color = rating), alpha = 0.55, stroke = 0.7) + 
  scale_fill_manual(values = c("#fcdd60", "#95d244")) +
  scale_size_area(max_size = 7) +
  MetBrewer::scale_color_met_c("Tam") +
  labs(
    title = "Bob's Burgers' dialogue shift",
    subtitle = str_wrap("In 2022, the show dramatically shortened its dialogue lines while maintaining its vocabulary richness. Seasons 13 & 14 show remarkably consistent line lengths around 23-24 characters, down from previous variations of 30-64.", width = 140),
    caption = "bobsburgersR package · Graphic: Georgios Karamanis",
    color = "IMDB rating",
    size = "Viewers (millions)",
    x = "Average number of characters per line of dialogue",
    y = "Unique lowercase words in episode"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#F6FBFF", color = NA),
    legend.position = "top",
    legend.title.position = "bottom",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(2, "lines"),
    axis.title = element_text(family = f1b),
    axis.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(lineheight = 1, size = 12, margin = margin(0, 0, 15, 0))
  )
  
