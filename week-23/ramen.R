library(tidyverse)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
# dupes <- ramen_ratings %>% group_by(brand, variety, style, country, stars) %>% filter(n() > 1)

shio <- ramen_ratings %>%
  filter(country == "Japan", str_detect(variety, "Shio")) %>% 
  mutate(n = 1:n())

# japanese chopstics are pointy!

ggplot(shio, aes(n, stars)) +
  geom_segment(aes(x = -20, y = 0.12, 
                   xend = 32, yend = 0.12),
               size = 2.7, lineend = "round", color = "#612711") +
  geom_segment(aes(x = -20, y = 0.12, 
                   xend = 20, yend = 0.12),
               size = 3, lineend = "square", color = "#612711") +
  geom_segment(aes(x = -20, y = 0.4, 
                   xend = 32, yend = 0.3),
               size = 2.7, lineend = "round", color = "#612711") +
  geom_segment(aes(x = -20, y = 0.4, 
                   xend = 20, yend = 0.3),
               size = 3, lineend = "square", color = "#612711") +
  geom_col(width = 0.3, fill = "#DBD067") +
  scale_y_reverse(limits = c(8, 0)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#0881A3")
  )

ggsave("./week-23/ramen.png", height = 5, width = 5)


