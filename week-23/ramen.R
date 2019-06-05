library(tidyverse)

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
  geom_text(aes(x = -20,  y = c(1.1, 2.1, 3.1, 4.1, 5.1),
                label = c("1 star", "2 stars", "3 stars", "4 stars", "5 stars")),
            color = "gray85", size = 2, hjust = 0,
            family = "IBM Plex Mono") +
  geom_segment(aes(x = -20, y = c(0.99, 1.99, 2.99, 3.99, 4.99),
                   xend = c(0.6, 1.6, 3.6, 9.6, 23.6), yend = c(0.99, 1.99, 2.99, 3.99, 4.99)),
               color = "gray85", size = 0.3, alpha = 0.5) +
  # 5 starred
  geom_text(data = shio, aes(x = n, y = -0.2,
                label = paste(brand, variety, sep = " - ")),
            size = 1.8, family = "IBM Plex Sans",
            hjust = 0, angle = 90, color = "gray85") +
    
  # ramen               
  geom_col(data = shio, aes(n, stars), width = 0.5, fill = "khaki") +
  
  scale_y_reverse(limits = c(8, -5), breaks = c(1, 2, 3, 4, 5),
  position = "right") +
  
  # title, subtitle and caption
  geom_text(aes(x = 32, y = 7.4, label = "Ratings of Japanese Instant Shio Ramen"),
            family = "IBM Plex Serif SemiBold", hjust = 1,
            color = "white", size = 4.5) +
  geom_text(aes(x = 32, y = 7.8, label = "Source: TheRamenRater.com | Graphic: Georgios Karamanis"),
            family = "IBM Plex Sans", hjust = 1,
            color = "white", size = 3) +
  
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#0881A3", color = "#0881A3"),
  )

ggsave("./week-23/ramen.png", height = 7, width = 5)


