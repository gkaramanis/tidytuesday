library(tidyverse)
library(geofacet)
library(cowplot)
library(ggimage)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

phones <- landline %>% 
  select(entity, year, landline_subs) %>% 
  left_join(mobile) %>% 
  mutate(decade = year %/% 10 * 10) %>% 
  group_by(entity, decade) %>% 
  mutate(
    median_landline_subs = median(landline_subs, na.rm = TRUE),
    median_mobile_subs = median(mobile_subs, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  distinct(entity, code, decade, median_landline_subs, median_mobile_subs) %>% 
  group_by(entity) %>% 
  fill(code) %>% 
  ungroup() %>% 
  group_by(entity, decade) %>% 
  mutate(
    most = if_else(median_landline_subs > median_mobile_subs, "landline", "smartphone"),
    img = if_else(!is.na(most), paste0(here::here("2020-week46", "img/"), most, ".png"), NULL)
    ) %>% 
  ungroup() %>% 
  left_join(world_countries_grid1, by = c("entity" = "name"))

ggplot(phones) +
  geom_image(aes(col, -row - decade * 2.5, image = img), size = 0.014, asp = 0.8) +
  geom_text(aes(-8, -10 - decade * 2.5, label = paste0("'", str_sub(decade, -2, -1), "s")), size = 10, stat = "unique", family = "Futura Bold", color = "darkseagreen", alpha = 0.8) +
  labs(
    title = toupper("The fall of landline"),
    subtitle = "Icon shows if the majority of phone subscriptions is landline or mobile,  median values by country and decade",
    caption = "Source: OurWorldInData.org | Graphic: Georgios Karamanis)"
    ) +
  coord_cartesian(clip = "off") +
  xlim(-14, 34) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, family = "Futura Bold", color = "grey20", size = 28, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(hjust = 0.5, family = "Atkinson Hyperlegible", color = "grey30", size = 7.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, family = "Atkinson Hyperlegible", color = "grey35", size = 7.5, margin = margin(0, 0, 0, 10)),
    plot.margin = margin(10, 10, 10, 10)
  ) 

ggsave(here::here("temp", paste0("phone-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 5.65)

