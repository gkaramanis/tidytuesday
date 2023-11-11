library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')

house_geoid <- house %>% 
  mutate(geoid = paste0(str_pad(state_fips, width = 2, pad = 0), str_sub(district, 2, 3))) %>% 
  filter(stage == "GEN") %>% 
  filter(year == 2022) %>%
  group_by(geoid, party) %>% 
  summarize(party_votes = sum(candidatevotes)) %>% 
  ungroup() %>% 
  group_by(geoid) %>% 
  slice_max(order_by = party_votes, n = 1)

house_cd <- sf::read_sf(here::here("2023/2023-week_45/data/HexCDv30wm/")) %>% 
  janitor::clean_names() %>% 
    left_join(house_geoid)

states <- sf::read_sf(here::here("2023/2023-week_45/data/HexSTv30wm/"))

states_lab <- states %>% 
  janitor::clean_names() %>% 
  mutate(
    i = row_number(),
    statename = if_else(i == 21, "Indiana", statename),
    stateab = if_else(i == 21, "IN", stateab)
    ) %>% 
  sf::st_centroid()

f1 <- "Founders Grotesk Condensed"
f2 <- "Charter"

ggplot() + 
  geom_sf(data = states, linewidth = 0.8, fill = NA) +
  geom_sf(data = house_cd, aes(fill = party), linewidth = 0.15, color = "white") +
  ggrepel::geom_text_repel(data = states_lab, aes(label = stateab, geometry = geometry), family = f1, stat = "sf_coordinates", bg.color = alpha("#F5F8FB", 0.5), size = 5, seed = 99) +
  scale_fill_manual(values = c("#367BB2", "#B53B2F"), na.value = "grey75") +
  coord_sf(expand = FALSE) +
  labs(
    title = "US House Election Results 2022",
    subtitle = "Party with the most votes (all candidates) by congressional district",
    caption = "Source: MIT Election Data and Science Lab · Shapefiles by Daniel Donner · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.63, 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "#F5F8FB", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold"),
    plot.subtitle = element_text(family = f2, size = 14),
    plot.caption = element_text(family = f2, margin = margin(20, 0, 10, 0), size = 10),
    plot.margin = margin(20, 20, 0, 20)
  )
  