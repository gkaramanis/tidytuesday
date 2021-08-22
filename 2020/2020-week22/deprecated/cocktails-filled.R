library(tidyverse)
# library(ggforce)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

# boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

glasses <- tribble(
  ~glass, ~v, ~r_top, ~r_bottom, ~h, ~h_top,
  "cocktail glass", 222, 11.4/2, 8.9/2, 7.1, 6.5,
  "collins glass", 360, 6.4/2, 6.4/2, 13.2, 11.2,
  "highball glass", 336, 7.6/2, 7/2, 12, 11,
  "old-fashioned glass", 303, 8.3/2, 8.3/2, 6.9, 5.6
) %>% 
  rowwise() %>% 
  mutate(
    x_inner = list(c(-r_top, r_top, r_bottom, -r_bottom, -r_top)),
    x_outer = list(x_inner * 1.01),
    y_inner = list(c(h_top, h_top, 0, 0, h_top)),
    y_outer = list(c(h_top, h_top, -(h - h_top), -(h - h_top), h_top)),
    th = atan(h / r_top - r_bottom)
  ) 

iba_cocktails <- cocktails %>%
  distinct(drink, glass, iba) %>% 
  filter(!is.na(iba)) %>%
  mutate(
    glass = tolower(glass)
    ) %>%
  group_by(iba) %>% 
  mutate(
    drink_x = row_number() %% 8 * 15,
    drink_y = 60 - row_number() %/% 8 * 20
  ) %>% 
  ungroup() %>% 
  left_join(glasses) %>% 
  unnest(c(x_inner, x_outer, y_inner, y_outer))


ggplot(iba_cocktails) +
  geom_polygon(aes(x = x_outer + drink_x, y = y_outer  + drink_y, group = drink), fill = "#a8ccd7", colour = "#a8ccd7", size = 0.5) +
  geom_polygon(aes(x = x_inner + drink_x, y = y_inner  + drink_y, group = drink, fill = iba)) +
  geom_text(aes(x = drink_x, y = drink_y - 4, label = drink), check_overlap = TRUE, family = "IBM Plex Mono", hjust = 0.5, size = 1.5) +
  coord_fixed() +
  facet_wrap(vars(iba)) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    legend.position = "none"
  ) 

ggsave(here::here("2020-week22", "plots", "temp", paste0("cocktails-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 16, height = 6)

