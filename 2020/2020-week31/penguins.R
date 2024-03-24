library(tidyverse)
library(ggforce)

# source("/Users/georgios/Documents/Projects/R/ggembed/ggembed-functions.R")

penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

med <- penguins.csv %>% 
  filter(!is.na(bill_length_mm)) %>% 
  group_by(species) %>% 
  summarize(
    n = n(),
    median_length = median(bill_length_mm),
    sd_length = sd(bill_length_mm),
    median_depth = median(bill_depth_mm),
    sd_depth = sd(bill_depth_mm),
    median_flipper = median(flipper_length_mm),
    sd_flipper = sd(flipper_length_mm)
    ) %>% 
  group_by(species) %>% 
  mutate(species_n = 4 - cur_group_id()) %>% 
  ungroup()

head_points <- read_csv(here::here("2020", "2020-week31", "data", "head.csv")) %>% 
  mutate(y = 400 - y,
         y = round(y / 5.5) - 32,
         x = round(x / 5.5) - 90,
         h = row_number()) 

flipper_points <- read_csv(here::here("2020", "2020-week31", "data", "flipper.csv")) %>% 
  mutate(
    x = round(x / 1.61) - 370,
    y = (200 - round(y / 1.61)) - 200,
    f = row_number())
 
penguin <- head_points %>% 
  arrange(-h) %>% 
  bind_rows(flipper_points) %>% 
  bind_rows(replicate(2, ., simplify = FALSE)) %>%
  mutate(species = rep(med$species, each = 40)) %>% 
  left_join(med) %>% 
  mutate(x = if_else(h == 13, median_length, x, missing = x),
         y = if_else(h == 10, median_depth/2, y, missing = y),
         y = if_else(h == 15, -median_depth/2, y, missing = y),
         x = if_else(f == 11, -131 - median_flipper, x, missing = x),
         x = if_else(f == 10 | f == 12, -121 - median_flipper, x, missing = x)
         ) %>%
  group_by(species) %>% 
  mutate(species_n = 4 - cur_group_id()) %>% 
  ungroup() %>% 
  mutate(y = if_else(h > 1 & h < 7 & species == "Adelie", y * 1.4, y, missing = y))

whites <- read_csv(here::here("2020", "2020-week31", "data", "whites.csv")) %>% 
  group_by(species_n) %>% 
  mutate(w = row_number())

recorded <- med %>% 
  select(species, n) %>%
	group_by(species) %>% 
  mutate(species_n = 4 - cur_group_id()) %>%
	ungroup() %>%
  rowwise() %>% 
  mutate(x = list((1:n - 1) %% 15),
         y = list(((1:n - 1) %/% 15))) %>% 
  unnest(c(x, y))

fam1 = "Founders Grotesk Condensed"
fam1b = "Founders Grotesk Condensed Bold"
fam2 = "Proxima Nova Extrabold"
fam2l = "Proxima Nova Light"

ggplot(penguin) +
# Right flipper ------------------------------------------------------------
  geom_bspline_closed(data = subset(penguin, !is.na(f)), aes((-x - 240) * 0.6, y + 300 * species_n, group = species), n = 300, fill = "grey20") + # subset the flipper from all the points, flip and compress
# Left flipper and head ---------------------------------------------------
  geom_bspline_closed(aes(x, y + 300 * species_n, group = species), n = 300, fill = "grey10") +
# White parts -------------------------------------------------------------
  geom_bspline_closed(data = whites, aes(x, y + 300 * species_n, group = interaction(species_n, part)), fill = "white") +
# Black band for chinstrap
  annotate("path", x = c(-55, -30, 0), y = c(600, 590, 580), size = 0.8, lineend = "butt", colour = "black") +
# Eyes --------------------------------------------------------------------
  geom_ellipse(aes(x0 = -32, y0 = 300 * species_n, a = 6, b = 4, angle = 0), fill = "black", colour = "white") +
# Flipper lines and labels ------------------------------------------------
  geom_errorbar(data = med, aes(xmin = -131 - median_flipper, xmax = -131, y = -50 + 300 * species_n, width = 10)) +
	geom_text(data = med, aes(x = -131 - median_flipper / 2, y = -25 + 300 * species_n, label = paste0(median_flipper, " mm")), family = fam1, size = 5) +
  geom_errorbar(data = med, aes(xmin = 0, xmax = median_length, y = 25 + 300 * species_n, width = 10)) +
  geom_text(data = med, aes(x = median_length / 2, y = 50 + 300 * species_n, label = paste0(median_length, " mm")), size = 4, family = fam1) +
  geom_errorbar(data = med, aes(x = 55, ymin = 300 * species_n - median_depth/2, ymax = 300 * species_n + median_depth/2, width = 10)) + 
  geom_text(data = med, aes(x = 70, y = 300 * species_n, label = paste0(median_depth, " mm")), size = 4, hjust = 0, family = fam1) +
# Recorded tiles ----------------------------------------------------------
  geom_tile(data = recorded, aes(x * 20 + 300, -y * 20 + 300 * species_n, width = 19, height = 19), colour = NA, fill = "grey10") +
  geom_text(data = recorded, aes(300, -30 + 300 * species_n, label = n), check_overlap = TRUE, colour = "white", size = 14, family = fam1b, hjust = 0) +
  annotate("text", 290, 930, label = "Recorded penguins by species", hjust = 0, family = fam2l, size = 4) +
# Species labels ----------------------------------------------------------
  geom_text(aes(-350, 40 + 300 * species_n, label = species), check_overlap = TRUE, hjust = 0, family = fam2, size = 10) +
# Scales, theme, etc ------------------------------------------------------
  labs(title = "Palmer Penguins",
       subtitle = "Median length of flipper, length and depth of bill,\nof 342 penguins recorded between 2007 and 2009",
       caption = "Source: Dr. Kristen Gorman and the Palmer Station, Antarctica LTER | Graphic: Georgios Karamanis") +  
  # coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(colour = "grey10"),
    plot.title = element_text(family = fam2, size = 30, hjust = 0.5, margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_text(family = fam2l, size = 12, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(family = fam2l, size = 7, hjust = 0.5, margin = margin(10, 0, -15, 0))
  ) 

# ggsave(here::here("2020-week31", "plots", "penguins-ggembed.png"), dpi = 320, height = 8, width = 8)