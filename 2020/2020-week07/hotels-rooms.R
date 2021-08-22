# The code has many values written out as calculations and not just numbers, so that I could understand where to draw all the shapes
# In many places I repeat myself
# Hope you have fun, you've been warned! :)

library(tidyverse)
library(cowplot)
library(here)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# hexagon
hex_x <- c(0, 0.87, 0.87, 0, -0.87, -0.87)
hex_y <- c(-1, -0.5, 0.5, 1, 0.5, -0.5)

# calculate sums and percentages by month
rooms <- hotels %>% 
  group_by(hotel, arrival_date_month) %>%
  summarise_at(.vars = vars(adults, children, babies),
               .funs = c(sum = "sum"), na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(
      month = fct_relevel(arrival_date_month,
                  c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
    adults_pct = adults_sum/max(adults_sum),
    children_pct = children_sum/max(children_sum),
    babies_pct = babies_sum/max(babies_sum)
  ) %>% 
  # calculate hexagon points for every row
  rowwise() %>%
  mutate(
    # this is the grey "room"
    x = list(hex_x),
    y = list(hex_y),
    # adults box
    x_a = list(hex_x * 0.9),
    y_a = list(c(-1, -0.5, -0.5 + adults_pct, 0 + adults_pct, -0.5 + adults_pct, -0.5)* 0.9),
    # children box
    x_c = list(hex_x * 0.4 - 0.0),
    y_c = list(c(-1, -0.5, -0.5 + children_pct, 0 + children_pct, -0.5 + children_pct, -0.5)* 0.4 + 0.0),
    # babies box
    x_b = list(hex_x * 0.4 + 0.35),
    y_b = list(c(-1, -0.5, -0.5 + babies_pct, 0 + babies_pct, -0.5 + babies_pct, -0.5)* 0.4 - 0.2)
    ) %>% 
  unnest(cols = c(x, y, x_a, x_c, y_c, y_a, x_b, y_b))

# plot 1 for city hotel
city <- rooms %>%
  filter(hotel == "City Hotel") %>%
  ggplot() +
  # "room"
  geom_polygon(aes(x, y), alpha = 1, fill = "grey85") +
  geom_text(aes(-0.1, 1.05, label = month), angle = 30, hjust = 1, check_overlap = TRUE, size = 3.5, family = "JetBrains Mono Regular", color = "grey20") +
  # adults
  geom_polygon(aes(x_a, y_a), alpha = 1, fill = "#6F8BA0") +
  geom_text(aes(0, adults_pct * 0.9 - 0.1, label = adults_sum), angle = 30, hjust = 1, check_overlap = TRUE, size = 3, family = "JetBrains Mono Regular", color = "grey92") +
  # geom_text(aes(0, y_a, label = adults_sum), angle = 35, size = 1) +
  # children
  geom_polygon(aes(x_c, y_c), alpha = 1, fill = "#4A5F71") +
  geom_text(aes(0, children_pct * 0.4 - 0.1, label = children_sum), angle = -30, hjust = 0, check_overlap = TRUE, size = 2.5, family = "JetBrains Mono Regular", color = "grey92") +
  # babies
  geom_polygon(aes(x_b, y_b), alpha = 1, fill = "#DFEBF1") +
  geom_text(aes(0.35, babies_pct * 0.4 - 0.3, label = babies_sum), angle = -30, hjust = 0, check_overlap = TRUE, size = 2.5, family = "JetBrains Mono Regular", color = "grey20") +
  # theme and stuff
  coord_fixed() +
  theme_void(base_family = "JetBrains Mono Regular") +
  facet_wrap(vars(month), ncol = 6) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(10, 20, 0, 20)
  )

# plot for resort hotel
resort <- rooms %>%
  filter(hotel == "Resort Hotel") %>%
  ggplot() +
  # "room"
  geom_polygon(aes(x, y), alpha = 1, fill = "grey85") +
  geom_text(aes(-0.1, 1.05, label = month), angle = 30, hjust = 1, check_overlap = TRUE, size = 3, family = "JetBrains Mono Regular", color = "grey20") +
  # adults
  geom_polygon(aes(x_a, y_a), alpha = 1, fill = "#6B8A8D") +
  geom_text(aes(0, adults_pct * 0.9 - 0.1, label = adults_sum), angle = 30, hjust = 1, check_overlap = TRUE, size = 2.5, family = "JetBrains Mono Regular", color = "grey92") +
  # children
  geom_polygon(aes(x_c, y_c), alpha = 1, fill = "#4B5453") +
  geom_text(aes(0, children_pct * 0.4 - 0.1, label = children_sum), angle = -30, hjust = 0, check_overlap = TRUE, size = 2.5, family = "JetBrains Mono Regular", color = "grey92") +
  # babies
  geom_polygon(aes(x_b, y_b), alpha = 1, fill = "#ADD0B5") +
  geom_text(aes(0.35, babies_pct * 0.4 - 0.3, label = babies_sum), angle = -30, hjust = 0, check_overlap = TRUE, size = 2.5, family = "JetBrains Mono Regular", color = "grey20") +
  # themes and stuff
  coord_fixed() +
  theme_void(base_family = "JetBrains Mono Regular") +
  facet_wrap(vars(month), ncol = 6) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0, 20, 20, 20)
  )

# plot for legend, labels and caption
legend <- ggplot() +
  # hexagons
  annotate("polygon", x = hex_x, y = hex_y, fill = "grey85", color = "grey85") +
  annotate("polygon", x = hex_x * 0.9, y = c(-1, -0.5, 0.5 - 0.1, 1 - 0.1, 0.5 - 0.1, -0.5) * 0.9, fill = "grey60", color = "grey60") +
  annotate("polygon", x = hex_x * 0.4 - 0.0, y = c(-1, -0.5, -0.5 + 0.7, 0.7, -0.5 + 0.7, -0.5)* 0.4 + 0.0, fill = "grey40", color = "grey40") +
  annotate("polygon", x = hex_x * 0.4 + 0.35, y = c(-1, -0.5, -0.5 + 0.2, 0 + 0.2, -0.5 + 0.2, -0.5)* 0.4 - 0.2, fill = "grey75", color = "grey75") +
  # lines
  annotate("segment", x = -0.87 * 0.9, y = 0.4 * 0.9, xend = -1.87 * 0.9, yend = 0.4 * 0.9, color = "grey60") +
  annotate("segment", x = 0.87 * 0.4, y = 0.2 * 0.4, xend = 0.87 * 0.5 + 1.25, yend = 0.2 * 0.4, color = "grey40") +
  annotate("segment", x = 0.87 * 0.4 + 0.35, y = -0.3 * 0.4 - 0.2, xend = 0.87 * 0.5 + 1.25, yend = -0.3 * 0.4 - 0.2, color = "grey75") +
  # text
  annotate("text", x = -1.87 * 0.9 - 0.2, y = 0.4 * 0.9, label = paste0("Adult guests\n", str_wrap("The number inside the box shows the number of guests for the month, while the height of the box is relative to the month with the highest number of guests in both hotels", 44)), hjust = 1, family = "JetBrains Mono Regular") +
  annotate("text", x = 0.87 * 0.5 + 1.45, y = 0.2 * 0.4, label = "Children", hjust = 0, family = "JetBrains Mono Regular") +
  annotate("text", x = 0.87 * 0.5 + 1.45, y = -0.3 * 0.4 - 0.2, label = "Babies", hjust = 0, family = "JetBrains Mono Regular") +
  # plot labels
   annotate("text", x = 7.5, y = 0.9, label = "City hotel", hjust = 1, family = "IBM Plex Sans Bold", size = 10, color = "#4A5F71") +
  annotate("text", x = 7.5, y = -0.7, label = "Resort hotel", hjust = 1, family = "IBM Plex Sans Bold", size = 10, color = "#ADD0B5") +
  annotate("text", x = 7.5, y = 0.05, label = "Data: Antonio, Almeida and Nunes, 2019\nGraphic: Georgios Karamanis", hjust = 1, family = "JetBrains Mono Regular", size = 3, color = "grey30") +
  coord_fixed(clip = "off", xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_void()

# combine all plots and save
plot_grid(city, legend, resort,  rel_heights = c(1, 0.3, 1),
label_fontfamily = "JetBrains Mono Regular", hjust = 1, label_x = 0.96, label_y = 1, ncol = 1) 

ggsave(here::here("2020-week07", "plots", "hotels-rooms.png"), dpi = 320,
         width = 12, height = 12)
                 
