library(tidyverse)

allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

hsl <- allShades %>% 
  select(hex, hue, sat, lightness)

exa_names <- allCategories %>%
  filter(categories == "name" & str_detect(brand, "Exa")) %>%
  mutate(name = fct_reorder(name, -lightness)) %>%
  left_join(hsl)
  

f1bi = "Publico Headline Bold Italic"

ggplot(exa_names) +
  geom_text(aes(x = hue, y = lightness, label = name, color = hex),
            hjust = 0, size = 3, family = f1bi, angle = 30) +
  scale_x_binned() +
  scale_y_binned() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal() 

ggsave(here::here("temp", paste0("makeup-shades-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 7, height = 7)

