library(tidyverse)
library(spatstat)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')

heart_fun <- function(n) {
  t <- c(seq(0, 2 * pi, length.out = n), 0)
  
  out <- data.frame(
    x = c(16 * sin(t) ^ 3),
    y = 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
  )
  out <- as.matrix(out)
  out <- list(out)
  st_polygon(out)
}

heart <- heart_fun(100) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  select(x = 1, y = 2) %>% 
  mutate(x = -x, y = y) %>% 
  mutate(i = row_number()) 

heart_hex <- data.frame(hextess(owin(poly = heart), 0.6, trim = FALSE)) %>% 
  select(Tile, x, y)

heart_hex_i <- heart_hex %>% 
  group_by(Tile) %>% 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y)
  ) %>% 
  arrange(mean_x, mean_y) %>% 
  mutate(i = row_number()) %>% 
  select(Tile, i)

hh <- heart_hex %>% 
  left_join(heart_hex_i)

candy <- gifts_age %>% 
  select(Age, Candy) %>% 
  rowwise() %>% 
  mutate(i = list(1:(Candy * max(hh$i) / 100))) %>% 
  unnest(i) %>% 
  left_join(hh, by = "i") %>% 
  group_by(Age) %>% 
  mutate(label_x = min(x) + (max(x) - min(x)/2)) %>% 
  ungroup()

f1 <- "Radio Canada"
f2 <- "Domine"

ggplot() +
  geom_polygon(data = hh, aes(x, y, group = i), fill = "black", color = "black", linewidth = 3) +
  geom_polygon(data = hh, aes(x, y, group = i), fill = "grey20", color = "grey50", linewidth = 0.2) +
  geom_polygon(data = candy, aes(x, y, group = i), fill = "brown3", linewidth = 0.2, color = "coral") +
  geom_text(data = candy, aes(label_x, 1.5, label = paste0(Candy, "%")), stat = "unique", color = "white", size = 8, fontface = "bold", family = f1) +
  facet_wrap(vars(paste0(Age, " years")), strip.position = "top") +
  coord_fixed(clip = "off") +
  labs(
    title = "Average percentage spending on Valentine's day candy by age",
    # subtitle = "Subtitle",
    caption = "Source: NRF · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(family = f2, face = "bold", size = 14, color = "grey20", margin = margin(0, 0, 8, 0)),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = margin(20, 20, 15, 20),
    plot.title = element_text(face = "bold", size = 18, color = "#413ABD", hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(color = "#413ABD", hjust = 0.5, margin = margin(15, 0, 0, 0))
  )
  
