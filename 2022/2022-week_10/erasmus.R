library(tidyverse)
library(camcorder)
library(treemapify)
library(sf)
library(patchwork)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

erasmus_rec <- erasmus %>% 
  # filter(academic_year == "2019-2020") %>%
  select(academic_year, sending_country_code, receiving_country_code, participants) %>% 
  filter(sending_country_code != receiving_country_code) %>% 
  group_by(academic_year, sending_country_code, receiving_country_code) %>% 
  summarise(n = sum(participants)) %>% 
  ungroup() %>% 
  pivot_longer(sending_country_code:receiving_country_code, names_to = "direction", values_to = "country") %>%
  mutate(direction = str_extract(direction, ".+?(?=_)")) %>% 
  group_by(academic_year, direction, country) %>% 
  summarise(total = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "direction", values_from = "total") %>% 
  mutate(
    receiving = replace_na(receiving, 0),
    r = sending / receiving
    ) %>% 
  group_by(academic_year) %>% 
  arrange(-receiving) %>% 
  mutate(
    rank_rec = row_number(),
    top = if_else(rank_rec < 6, TRUE, FALSE)
    ) %>% 
  ungroup()

f1 <- "October Condensed Devanagari"
f2 <- "DIN Condensed"

pal <- colorspace::desaturate(RColorBrewer::brewer.pal(5, "Dark2"), 0.15)

p1 <- erasmus_rec %>% 
  filter(academic_year == "2019-2020") %>%
  mutate(academic_year = "Erasmus+\nStudents and staff received\nby country in 2019-2020\n\n(top 5 highlighted)") %>% 
  ggplot(aes(area = receiving, fill = ifelse(top, country, NA), label = paste0(country, "\n", receiving), subgroup = academic_year)) +
  geom_treemap(radius = unit(0, "pt"), color = "grey97") +
  geom_treemap_text(family = f1, color = "grey97", size = 25) +
  geom_treemap_subgroup_text(place = "bottomleft", grow = FALSE, alpha = 1, color = "white", family = f2, size = 28, padding.x = unit(3, "mm"), padding.y = unit(4, "mm"),) +
  scale_fill_manual(values = (pal), na.value = "#6E6F71") +
  # facet_wrap(vars(academic_year)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_blank()
  )

europe_shp <- sf::read_sf(here::here("2022/2022-week_10/data/NUTS_RG_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")) %>% 
  janitor::clean_names() %>% 
  filter(levl_code == 0)

p2 <- ggplot(europe_shp) +
  geom_sf(size = 0.1, fill = "#5A9416", color = "#2E5200") +
  geom_sf_text(aes(label = cntr_code), size = 2, family = f1, color = "grey80") +
  coord_sf(xlim = c(2.5e6, 6.5e6), ylim = c(1.3e6, 5.3e6), datum = "EPSG:3035") +
  labs(caption = "Source: data.europa.eu · Graphic: Georgios Karamanis") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.caption = element_text(family = f1, size = 9, hjust = 0.5, color = "grey97")
  )

p1 +
  # inset_element(p2, 0.05, 0.01, 0.37, 0.37) # Alternative placing (bottom right)
  inset_element(p2, 0.73, 0, 1, 0.3)

