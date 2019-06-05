library(tidyverse)
library(ggrepel)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
# dupes <- ramen_ratings %>% group_by(brand, variety, style, country, stars) %>% filter(n() > 1)

shio <- ramen_ratings %>%
  filter(country == "Japan", str_detect(variety, "Shio")) %>% 
  arrange(., stars) %>%
  mutate(n = 1:n())

ggplot() +
# chopstics
  geom_polygon(aes(x = c(-30, 32, 32, -30), y = c(0.02, 0.02, 0.1, 0.3)),
               fill = "firebrick4") +
  geom_polygon(aes(x = c(-30, 32, 32, -30), y = c(0.35, 0.12, 0.2, 0.63)),
               fill = "firebrick4") +

# stars
  geom_text(aes(x = -12,  y = c(0.99, 1.99, 2.99, 3.99, 4.99),
                label = c(1, 2, 3, 4, 5)),
            color = "gray85", size = 2) +
  geom_segment(aes(x = -10, y = c(0.99, 1.99, 2.99, 3.99, 4.99),
                   xend = c(1, 2, 4, 10, 24), yend = c(0.99, 1.99, 2.99, 3.99, 4.99)),
               color = "gray85", size = 0.3) +
    
# ramen               
  geom_col(data = shio, aes(n, stars), width = 0.5, fill = "khaki") +
  geom_text(data = shio, aes(x = n, y = 0.12, label = variety),
            size = 1, angle = 270, hjust = 0) +
  
  scale_y_reverse(limits = c(7, 0), breaks = c(1, 2, 3, 4, 5),
  position = "right") +

  theme_void() +
  theme(
    panel.background = element_rect(fill = "#0881A3", color = "#0881A3"),
    text = element_text(family = "IBM Plex Serif", size = 2),
    panel.grid = element_blank(),
  )

ggsave("./week-23/ramen.png", height = 4, width = 5)


