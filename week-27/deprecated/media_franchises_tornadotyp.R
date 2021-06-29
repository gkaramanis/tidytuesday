library(tidyverse)
library(ggimage)
library(here)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

top_games <- media_franchises %>%
  filter(original_media == "Video game" &
           revenue_category == "Video Games/Games") %>%
  distinct() %>% 
  mutate(
    franchise = str_replace_all(franchise, c("é" = "e", "&" = "AND")),
    coins = map2(5, revenue + 5, seq, by = 1),
    ) %>%
  arrange(year_created) %>% 
  mutate(
    revenue = ifelse((row_number() %% 2) == 1, revenue, -revenue),
    coins = ifelse((revenue > 0), coins, map(coins, function(x) x*-1))
         ) %>% 
  unnest(coins) 

coin_jpg <- here("week-27", "img", "coin.jpg")

top_games %>% 
  group_by(franchise) %>% 
  ggplot(aes(coins,
             factor(fct_reorder(franchise, year_created, .desc = TRUE)))) +
  geom_point() +
  geom_image(aes(image = coin_jpg), size = 0.015, asp = 1.8) +
  geom_text(aes(x = 0,
                label = paste(franchise, year_created, sep = "\n")),
            family = "Karmatic Arcade",
            size = 3,
            color = "purple", fill = "purple",
            stat = "identity",
            position = "identity",
            check_overlap = TRUE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "green"),
    text = element_text(family = "Karmatic Arcade"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) 

ggsave(here("week-27", "media_franchises.png"),
          dpi = 300)

