library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

a = 45 * pi / 180

art <- artists %>% 
  distinct(artist_name, artist_gender, artist_unique_id, year, moma_count_to_year, whitney_count_to_year) %>% 
  arrange(year) %>% 
  group_by(artist_unique_id) %>%
  mutate(
    x = moma_count_to_year,
    y = whitney_count_to_year,
    x2 = x * cos(a) + y * sin(a),
    y2 = -x * sin(a) + y * cos(a),
    start_year = if_else(year == min(year), TRUE, FALSE),
    end_year = if_else(year == max(year), TRUE, FALSE)
  ) %>% 
  ungroup()
  

f1 <- "Outfit"
f2 <- "Cochin"

ggplot(art) +
  geom_abline(slope = c(0, 1, -1), color = "steelblue", linewidth = 1, alpha = 0.6) +
  geom_line(aes(x2, y2, group = artist_unique_id), color = "grey85", alpha = 0.7) +
  ggrepel::geom_text_repel(data = . %>% filter(end_year & (moma_count_to_year > 15 | whitney_count_to_year > 15)), aes(x2, y2, label = artist_name), size = 3, hjust = 0, nudge_x = 0.5, family = f1, segment.size = 0.25, seed = 999) +
  geom_point(data = . %>% filter(end_year), aes(x2, y2, color = abs(moma_count_to_year - whitney_count_to_year), shape = artist_gender), size = 3) +
  MetBrewer::scale_color_met_c("Tam", direction = 1) +
  annotate("text", x = 40, y = -1, label = "Equal number of exhibitions", family = f1, color = "steelblue") +
  annotate("text", x = 22, y = 24.5, label = "Exhibitions held only\nby The Whitney", family = f1, angle = 45, lineheight = 0.9, color = "steelblue") +
  annotate("text", x = 37, y = -39.5, label = "Exhibitions held\nonly by MoMA", family = f1, angle = -45, lineheight = 0.9, color = "steelblue") +
  annotate("text", x = 30, y = -6, label = "More exhibitions in total →", family = f1, fontface = "bold", color = "steelblue", size = 5) +
  annotate("text", x = 30, y = 3, label = "⬉\nEvolution of exhibitions\nthrough the years", family = f1, color = "grey70", lineheight = 0.9, hjust = 0) +
  annotate("text", x = -5, y = -30, label = "Exhibitions held by\nThe Whitney and MoMA", hjust = 0, size = 8.5, family = f2, fontface = "bold", lineheight = 1) +
  annotate("text", x = -5, y = -37, label = "For artists in Janson’s History of Art\nand Gardner’s Art Through the Ages\n1926—2020", hjust = 0, size = 5, family = f1, lineheight = 1.1, color = "grey20") +
  annotate("text", x = -5, y = -45, label = "Source: Lemus S, Stam H (2022). arthistory: Art History Textbook Data\nGraphic: Georgios Karamanis", hjust = 0, family = f1, color = "grey50", lineheight = 1) +
  scale_shape_manual(values = c(4, 18, 3)) +
  coord_fixed(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
