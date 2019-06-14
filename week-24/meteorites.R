library(tidyverse)
library(ggrepel)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

top10 <- meteorites %>%
  top_n(10, mass) %>%
  arrange(-mass) %>%
  mutate(x = (row_number()- 1) %% 5,
         y = ((row_number() - 1) %/% 5)
         )
# stars
stars <- data.frame(s = runif(100, min = 0.1, max = 0.7))

# world without Antarctica
world <- map_data("world") %>%
  filter(group < 32 | group > 132)

ggplot(top10, aes(x = x, y = y - 0.1)) +
  # stars
  geom_jitter(data = stars,
              aes(2, -1.2), width = 2.5, height = 1,
              size = stars$s, color = "white", alpha = stars$s) +
  
  # meteorites          
  geom_point(aes(size = mass), color = "darkorange") +
  
  # meteorite name
  geom_text(aes(y = y + 0.22,
                label = toupper(name)), size = 2.5, color = "white",
            family = "IBM Plex Sans Bold") +
  # meteorite mass
  geom_text(aes(y = y + 0.31,
                label = paste(mass/1000000, "tons", sep = " ")), size = 2.5, color = "orange",
            family = "IBM Plex Sans Bold") +
  # year
  geom_text(aes(y = y + 0.39,
                label = year), size = 2.5, color = "white",
            family = "IBM Plex Sans Italic") +
            
  # title         
  geom_text(
    aes(x = 2, y = -1.5,
        label = "THE TEN\nBIGGEST\n\nON EARTH"),
    size = 10, hjust = 0.5, lineheight = 0.8,
    color = "white", family = "IBM Plex Sans Bold"
  ) +
  geom_text(
    aes(x = 2, y = -1.358,
        label = "METEORITES"),
    size = 10, hjust = 0.5, lineheight = 0.8,
    color = "orange", family = "IBM Plex Sans Bold"
  ) +
  geom_text(
    aes(x = 2, y = -0.7,
        label = "Name, mass and year found or observed\nRanked by mass"),
    size = 3, hjust = 0.5, lineheight = 0.8,
    color = "white", family = "IBM Plex Sans Light"
  ) +
  
  # map
  geom_polygon(data = world, aes(2 + long/100,
                                 2.5 - lat/120,
                                 group = group),
               fill = "grey50", color = "grey30", size = 0.05) +
  geom_point(aes(2 + long/100, 2.5 - lat/120),
    alpha = 1, color = "darkorange") +
  geom_text_repel(aes(2 + long/100,
                2.5 - lat/120,
                label = name), color = "white",
                family = "IBM Plex Sans Italic",
            size = 2) +
  # caption
  geom_text(aes(x = 2, y = 3.5,
                label = "Source: NASA | Graphic: Georgios Karamanis"),
            color = "grey50", family = "IBM Plex Sans Light", size = 1.8) +
  
  scale_y_reverse() +
  coord_cartesian(xlim = c(-0.5, 4.5),
                  ylim = c(3.5,-2)) +
  scale_size(range = c(7, 20)) +

  theme_void() +
  theme(plot.background = element_rect(color = "midnightblue",
                                        fill = "midnightblue"),
        legend.position = "none"
        ) +
  
  ggsave("./week-24/meteorites.png",
         height = 8,
         width = 5)
