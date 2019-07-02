library(tidyverse)
library(ggimage)
library(here)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
coin_img <- here("week-27", "img", "coin.png")

top_games <- media_franchises %>%
  filter(original_media == "Video game" &
           revenue_category == "Video Games/Games") %>%
  distinct() %>% 
  top_n(10, revenue) %>% 
  mutate(
    # franchise = str_replace_all(franchise, c("é" = "e", "&" = "AND")),
    franchise = str_replace_all(franchise, " (?=\\w)", "\n"),
    coins = map2(0, revenue, seq, by = 1),
    # year_created = case_when(
    #   franchise == "Final Fantasy" ~ 1987.7,
    #   T ~ year_created
    # )
    ) %>%
  arrange(year_created) %>%
  unnest(coins) 

## original total revenue by all media types, not just the games themselves!
top_games %>% 
  group_by(franchise) %>% 
  ggplot(aes(coins,
              factor(fct_reorder(franchise, year_created, .desc = TRUE)))) +
  geom_image(aes(image = coin_img), size = 0.03, asp = 0.8) +
  scale_x_continuous(labels = c("0", "10B", "20B", "30B")) +
  # scale_y_reverse(
    # breaks = top_games$year_created
    # ) +
  # coord_cartesian(xlim = c(-16, 31)) +
  # geom_text(aes(x = -0.95, y = year_created + 0.05,
  #               label = franchise), hjust = 1, color = "black",
  #           family = "Press Start 2P", size = 3) +
  # geom_text(aes(x = -1, y = year_created,
  #          label = franchise), hjust = 1, color = "white",
  #          family = "Press Start 2P", size = 3) +
  labs(
    title = "Top 10 video games with the higher revenue",
    subtitle = "shown only revenue of games",
    caption = "Source: Wikipedia | Graphic: Georgios Karamanis",
    x = "Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#5099FF", color = "#5099FF"),
    text = element_text(family = "Press Start 2P"),
    axis.text.y = element_text(color = "white"),
    axis.text.x = element_text(color = "yellow"),
    axis.title.x = element_text(color = "yellow"),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  
  ggsave(here("week-27", "media_franchises.png"),
          dpi = 300, height = 8, width = 6)

