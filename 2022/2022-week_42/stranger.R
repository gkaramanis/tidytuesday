library(tidyverse)
library(ggtext)
library(shadowtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')

dialogue <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

lyrics <- dialogue %>% 
  filter(str_detect(dialogue, "♪"))

songs <- dialogue %>% 
  filter(str_detect(raw_text, '\\[\\"') & str_detect(raw_text, "playing"))

words <- dialogue %>% 
  filter(!is.na(dialogue))

f1 <- "Outfit"
f2 <- "Butler"

ggplot() +
  # Dialogue
  geom_segment(data = words, aes(x = start_time, xend = end_time, y = as.numeric(paste0(season, episode)), yend = as.numeric(paste0(season, episode))), color = "#44425B", size = 2) +
  # Lyrics
  geom_segment(data = lyrics, aes(x = start_time, xend = end_time, y = as.numeric(paste0(season, episode)), yend = as.numeric(paste0(season, episode))), color = "#336CE2", size = 2) +
  # Song titles
  geom_point(data = songs, aes(x = start_time, y = as.numeric(paste0(season, episode))), color = "#ff1515", shape = "♫", size = 2) +
  # Episode numbers
  geom_text(data = words, aes(x = -10, y = as.numeric(paste0(season, episode)), label = ifelse(episode == 5, paste0(episode, "\nSeason ", season), episode)), stat = "unique", family = f1, size = 2.3, vjust = 1, color = "#9782DF") +
  # Season number
  # geom_text(data = words, aes(x = -10, y = as.numeric(paste0(season, episode)), label = episode), stat = "unique", family = f1, size = 2.2, vjust = 1, color = "grey30") +
  # Title
  annotate("shadowtext", x = 9700, y = 46, label = "Stranger Songs", family = f2, fontface = "bold", size = 10, hjust = 0, color = "#1e193c", bg.color = "#ff1515", bg.r = 0.03) +
  # Subtitle
  annotate("richtext", x = 9540, y = 48,
           label = "Songs in the Stranger Things script<br><br>
           <span style='color:#ff1515'>♫</span> Song title mentioned in the script<br> 
           <span> </span><span style='color:#3a5fe5'>**|**</span><span> </span><span> </span>Song lyrics in the script<br>
           <span> </span><span style='color:#44425B'>**|**</span><span> </span><span> </span>Dialogue<br><br>
           <span style='color:#9782DF; font-size:13px'>Source: 8flix.com · Graphic: Georgios Karamanis</span>",
           family = f1, size = 4.5, hjust = 0, vjust = 1, color = "grey90", fill = NA, label.color = NA) +
  scale_x_continuous(limits = c(-10, 10000), breaks = c(0, 30, 60, 90, 120) * 60, labels = paste0(c(0, 30, 60, 90, 120), "'")) +
  scale_y_continuous(limits = c(0, 50)) +
  coord_polar(start = -pi/2, clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#1e193c", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#422288", size = 0.2),
    panel.grid.minor.x = element_line(color = "#422288", size = 0.1),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#9782DF"),
    plot.margin = margin(-10, -10, -10, -10)
  )
  
