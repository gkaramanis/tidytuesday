library(tidyverse)
library(camcorder)
library(geomtextpath)

gg_record(dir = "temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')

wannabe <- lyrics %>% 
  filter(song_name == "Wannabe")

# ggplot(wannabe) +
#   geom_text(aes(x = 0, y = line_number, label = line)) +
#   scale_y_reverse()

l = 200

t <- seq(5, -1, length.out = l) * pi

spiral <- data.frame(x = sin(t) * 1:(5*l), 
                     y = cos(t) * 1:(5*l),
                     text = paste(wannabe$line, collapse = " "))

ggplot(spiral, aes(x, y, label = text)) +
  geom_textpath(size = 3, vjust = 0.5, hjust = 0, include_line = FALSE) +
  coord_fixed(xlim = c(-(l + 500), l + 500), ylim = c(-(l + 500), l + 500), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )
