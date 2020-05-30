library(tidyverse)
library(magick)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

ingredients_tree <- cocktails %>% 
  select(ingredient) %>% 
  distinct(ingredient = tolower(ingredient)) %>% 
  mutate(
    ingr_x = rep(c(1, -1), n() / 2),
    ingr_y = rep(c(1:(n()/2)), 2) * 2
  )

black_cocktails <- cocktails %>% 
  mutate(ingredient = tolower(ingredient)) %>% 
  group_by(drink) %>%
  filter(any(ingredient == "dark rum")) %>%
  ungroup() %>%
  select(drink, iba, ingredient_number, ingredient) %>% 
  left_join(ingredients_tree)

ggplot(black_cocktails) +
  geom_segment(aes(x = 0, y = -50, xend = 0, yend = ingr_y - 20), size = 0.4, colour = "#E0B35B") +
# left --------------------------------------------------------------------
  geom_curve(data = subset(iba_cocktails, ingr_x < 0), aes(x = 0, y = ingr_y - 20, xend = ingr_x * 40, yend = ingr_y), size = 0.4, colour = "#E0B35B") +
  geom_curve(data = subset(iba_cocktails, ingr_x < 0), aes(x = 0, y = ingr_y - 20, xend = ingr_x * 40, yend = ingr_y), curvature = -0.5, size = 0.4, colour = "#E0B35B") +
  geom_text(data = subset(iba_cocktails, ingr_x < 0), aes(x = ingr_x * 40, y = ingr_y, label = ingredient), hjust = 1, nudge_x = -15, size = 2, family = "Produkt Light", colour = "grey70") +
# right -------------------------------------------------------------------
  geom_curve(data = subset(iba_cocktails, ingr_x > 0), aes(x = 0, y = ingr_y - 20, xend = ingr_x * 40, yend = ingr_y), curvature = -0.5, size = 0.4, colour = "#E0B35B") +
  geom_curve(data = subset(iba_cocktails, ingr_x > 0), aes(x = 0, y = ingr_y - 20, xend = ingr_x * 40, yend = ingr_y), size = 0.4, colour = "#E0B35B") +
  geom_text(data = subset(iba_cocktails, ingr_x > 0), aes(x = ingr_x * 40, y = ingr_y, label = ingredient), hjust = 0, nudge_x = 15, size = 2, family = "Produkt Light", colour = "grey70") +
# -------------------------------------------------------------------------
  coord_fixed(xlim = c(-200, 200)) +
  facet_wrap(vars(drink), nrow = 3, strip.position = "bottom") +
  labs(
    title = "Dark Rum Cocktails",
    caption = "Source: Kaggle | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(family = "Publico Headline Black", colour = "#D53434", margin = margin(0, 0, 10, 0)),
    plot.background = element_rect(fill = "#312921", colour = NA),
    plot.title = element_text(family = "Publico Headline Black Italic", size = 36, hjust = 0.5, margin = margin(10, 0, 0, 0), colour = "#FFD700"),
    plot.caption = element_text(family = "Produkt Light", size = 7, hjust = 0.5, margin = margin(30, 0, 0, 0), colour = "grey60"),
    plot.margin = margin(20, 35, 20, 35)
  ) +
  ggsave(here::here("2020-week22", "plots", "cocktails.png"), dpi = 320, width = 11.5, height = 8)

image_read(here::here("2020-week22", "plots", "cocktails.png")) %>% 
  image_trim(.) %>% 
  image_write(., here::here("2020-week22", "plots", "cocktails.png"))


