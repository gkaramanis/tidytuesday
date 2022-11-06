library(tidyverse)
library(lubridate)
library(ggfx)
library(ggpath)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

zombie <- horror_movies %>% 
  filter(if_any(c(original_title, title, overview, tagline), ~ str_detect(tolower(.x), "zombie"))) %>%
  filter(original_language == "en")

zombie_highl <- zombie %>% 
  filter(
    original_title == "Dawn of the Dead" | 
      original_title == "Night of the Living Dead" | 
      original_title == "The Return of the Living Dead" |
      original_title == "Shaun of the Dead"
         )

# Nathan Wright
# https://www.pexels.com/photo/zombies-behind-shabby-door-5435562/
img <- "https://images.pexels.com/photos/5435562/pexels-photo-5435562.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=3"

f1 <- "Outfit"
f2 <- "Sharpie"

ggplot(zombie) +
  as_reference(
    geom_histogram(aes(year(release_date)), binwidth = 1, fill = "deeppink1"),
    id = "histomask"
  ) +
  with_mask(
    geom_from_path(aes(path = img, x = 2009, y = 35), stat = "unique", width = 2.8, height = 2.8),
    mask = ch_alpha("histomask")
  ) +
  ggrepel::geom_text_repel(data = zombie_highl,
                           aes(x = year(release_date), y = -0.5,
                               label = paste0(title, ", ", year(release_date))),
                           nudge_y = 6,
                           segment.curvature = -1e-20,
                           family = f2,
                           fontface = "bold",
                           size = 4, 
                           bg.color = "grey97",
                           seed = 999,
                           box.padding = 0.4,
                           segment.size = 0.25,
                           color = "deeppink4"
                           ) +
  geom_point(data = zombie_highl, aes(x = year(release_date), y = -0.5), color = "deeppink1") +
  annotate("text", x = 1959, y = 20, label = "Zombie movies", size = 18, family = f2, fontface = "bold", hjust = 0, color = "deeppink2") +
  annotate("text", x = 1959, y = 24, label = "Number of english speaking movies\nin The Movie Datbase that have the\nword 'zombie' in their title or description", size = 6, family = f1, fontface = "bold", hjust = 0, color = "deeppink3", vjust = 1, lineheight = 0.9) +
  annotate("text", x = 1959, y = 31.5, label = "Source: The Movie Datbase · Image: cottonbro studio · Graphic: Georgios Karamanis", size = 3, family = f1, fontface = "bold", hjust = 0, color = "deeppink4", vjust = 1, lineheight = 0.9) +
  scale_y_reverse(limits = c(50, -8), breaks = seq(0, 50, 10), labels = seq(0, 50, 10), position = "right") +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold", size = 14, color = "deeppink4")
  )
  
