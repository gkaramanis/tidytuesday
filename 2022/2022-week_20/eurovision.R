library(tidyverse)
library(camcorder)
library(ggimage)
library(countrycode)
library(grid)
library(ggpp)
library(ggfx)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8.8, units = "in", dpi = 320)

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

rect_gr <- grid.rect(gp = gpar(col = NA, fill = linearGradient(colours = c("#161125", "#6A2D37"), x1 = 0.501, y1 = 0.2, x2 = 0.508, y2 = 1.05)))

circle_gr <- grid.circle(gp = gpar(col = NA, fill = radialGradient(colours = c("#701016", NA))))

circles <- data.frame(x = c(19, 0.25), y = c(9, 5)) %>% 
  mutate(grob = list(circle_gr))

euro22 <- eurovision %>% 
  filter(year == 2022 & section == "grand-final") %>% 
  arrange(rank) %>% 
  mutate(
    x = if_else(rank <= 13, 0, 8),
    y = if_else(rank <= 13, rank, rank - 13),
    flag = countrycode(artist_country, "country.name", "iso2c"),
    grob = list(rect_gr)
    )

masks <- euro22 %>% 
  select(x, y) %>% 
  mutate(mask = as.raster(matrix("black")))

# Fonts
f1 <- "Outfit"
f2 <- "Graphik"
f3 <- "Gotham"

systemfonts::register_font(
  name = "Outfit Medium",
  plain = "/Users/georgios/Library/Fonts/Outfit-Medium.ttf"
)

f1m <- "Outfit Medium"

# Plot
ggplot(euro22) +
  # Background
  geom_grob(data = circles, aes(x, y, label = grob), vp.height = c(1, 0.8), vp.width = 1) +
  # Background countries
  geom_grob(aes(x, y, label = grob), nudge_x = 3.5, vp.height = 0.046, vp.width = 0.3) +
  # Rank
  geom_tile(aes(x, y, width = 0.85, height = 0.85), fill = "#E37737") +
  geom_text(aes(x, y, label = str_pad(rank, 2, pad = "0")), size = 6, color = "white", family = f1, fontface = "bold") +
  # Flag
  as_reference(
    geom_tile(data = masks, aes(x + 1.05, y, height = 0.75, width = 1)),
    id = "mask"
  ) +
  with_mask(
    geom_flag(aes(x + 1.05, y, image = flag), asp = 1.4, size = 0.04),
    mask = ch_alpha("mask")
  ) +
  # Country
  geom_text(aes(x + 1.8, y, label = artist_country), size = 5.5, color = "white", hjust = 0, family = f2) +
  # Points
  geom_text(aes(x + 7.2, y, label = total_points), size = 5.5, color = "white", hjust = 1, family = f1m) +
  labs(
    title = "Eurovision 2022 Results",
    caption = "Source: Eurovision · Graphic: Georgios Karamanis"
  ) +
  scale_x_continuous(limits = c(-4, 19)) +
  scale_y_reverse(limits = c(15, -2)) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#0C0A16", color = NA),
    plot.title = element_text(color = "grey98", hjust = 0.5, family = f3, face = "bold", size = 40, margin = margin(50, 0, -60, 0)),
    plot.caption = element_text(size = 12, color = "grey98", hjust = 0.5, family = f3, margin = margin(-20, 0, 20, 0))
  )
  
