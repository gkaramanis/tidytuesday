library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  filter(category == "Medicine") %>% 
  mutate(nume = as.numeric(str_sub(prize_share, 1, 1)),
         deno = as.numeric(str_sub(prize_share, -1)),
         share = nume/deno,
         year = prize_year %% 10,
         decade = prize_year - 1900 - year) %>%
  group_by(prize_year) %>% 
  distinct(full_name, .keep_all = TRUE) %>% 
  mutate(n = row_number()) %>% 
  ggplot() + 
  geom_bar(aes(x = "", y = share, fill = n),
    stat = "identity", show.legend = FALSE
  ) +
  scale_fill_identity() +
  coord_polar("y") +
  facet_grid(decade ~ year) +
  labs(title = "Shared Nobel Prizes in Medicine",
       subtitle = "Cumulative lethal collisions, 1980-2017",
       caption = "\nSource: Kaggle | Graphic: Georgios Karamanis / @geokaramanis") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
    text = element_text(family = "IBM Plex Sans", size = 6),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(vjust = 2)
  ) 
