library(tidyverse)
library(rtrek)
library(trekcolors)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')

tmln <- tlBooks %>% 
  filter(!is.na(series)) %>% 
  group_by(series) %>% 
  summarise(
    min_year = min(year),
    max_year = max(year)
  ) %>% 
  mutate(
    x1 = if_else(min_year < -3000, -3000, min_year),
    x2 = if_else(min_year < -3000, -3000 - 100 * log(abs(min_year)), min_year),
    xend1 = if_else(max_year > 8000, 8000, max_year),
    xend2 = if_else(max_year > 8000, 8000 + 100 * log(max_year), max_year),
    same = if_else(x2 == xend2, TRUE, FALSE)
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(stSeries, by = c("series" = "abb")) %>% 
  arrange(-x2) %>% 
  mutate(y = row_number()) 
  
prim_y <- tlBooks %>% 
  distinct(series, primary_entry_year) %>% 
  right_join(tmln) %>% 
  select(series, primary_entry_year, y) %>% 
  mutate(
    prim = if_else(primary_entry_year > 8000, 8000 + 100 * log(primary_entry_year), primary_entry_year)
  ) %>% 
  filter(!is.na(primary_entry_year))

f1 <- "Outfit"
f2 <- "Exetegue"

ggplot(tmln) +
  annotate("segment", x = seq(2000, 2500, 100), xend = seq(2000, 2500, 100), y = 0.9, yend = 13.8, linewidth = 0.1, color = "white") +
  annotate("text", x = c(2000, 2300, 2500), y = 13.85, label = c(2000, 2300, 2500), color = "white", family = f1, size = 1.8, vjust = 0, hjust = c(1, 0.5, 0)) +
  # Extended lines
  geom_segment(aes(x = xend1, xend = xend2, y = y, yend = y, color = id), size = 2.5, lineend = "round", linetype = "12") +
  geom_segment(aes(x = x1, xend = x2, y = y, yend = y, color = id), size = 2.5, lineend = "round", linetype = "12") +
  # Solid line
  geom_segment(aes(x = x1, xend = xend1, y = y, yend = y, color = id), size = 2.5, lineend = "round") +
  # Point if same min and max year
  geom_point(data = tmln %>% filter(same == TRUE), aes(x = x2, y = y), size = 1.5) +
  # Primary entry years
  geom_point(data = prim_y, aes(x = prim, y = y), color = "white", size = 0.95) +
  # Min year
  geom_text(data = tmln %>% filter(same == FALSE), aes(x = x2, y = y, label = ifelse(min_year <= -2000, paste0(scales::number(abs(min_year)), " BCE"), min_year)), hjust = 1, nudge_y = 0.2, size = 3, family = f1, color = "#060B60") +
  # Max year
  geom_text(data = tmln %>% filter(same == FALSE), aes(x = xend2, y = y, label = ifelse(max_year >= 3000, paste0(scales::number(max_year), " CE"), max_year)), hjust = 0, nudge_y = 0.2, size = 3, family = f1, color = "#060B60") +
  # Year if same min and max year
  geom_text(data = tmln %>% filter(same == TRUE), aes(x = xend2, y = y, label = max_year), nudge_y = 0.2, size = 3, family = f1, color = "#060B60") +
  # Series name
  geom_text(aes(x = x2 + (xend2 - x2) / 2, y = y, label = id), nudge_y = -0.25, family = f1, fontface = "bold", color = "#060B60") +
  # Title
  annotate("text", x = c(-3400, 7000), y = 4.4, label = c("STAR\nTREK", "TIME\nLINE"), size = 50, family = f2, fontface = "bold", lineheight = 0.7, color = "#909083") +
  # Caption
  annotate("text", x = c(-3400, 7000), y = 1.25, label = c("Source: rtrek package", "Graphic: Georgios Karamanis"), family = f1, size = 3.2, fontface = "bold", color = "#656556") +
  # Annotation
  annotate("text", x = 2200, y = 13.5, label = toupper("• Year of the main events"), color = "white", family = f2, size = 5) +
  scale_color_trek("lcars_cardassian") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#CBCBD5", color = NA),
    plot.margin = margin(0, 20, 0, 60)
  )
  
