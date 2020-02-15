library(tidyverse)
library(cowplot)
library(here)

# library(gtsummary)
# tbl_summary(hotels)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

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
  rowwise() %>%
  mutate(
    x = list(c(0, 0.87, 0.87, 0, -0.87, -0.87)),
    y = list(c(-1, -0.5, 0.5, 1, 0.5, -0.5)),
    x_a = list(c(0, 0.87, 0.87, 0, -0.87, -0.87) * 0.9),
    y_a = list(c(-1, -0.5, -0.5 + adults_pct, 0 + adults_pct, -0.5 + adults_pct, -0.5)* 0.9),
    x_c = list(c(0, 0.87, 0.87, 0, -0.87, -0.87) * 0.4 - 0.0),
    y_c = list(c(-1, -0.5, -0.5 + children_pct, 0 + children_pct, -0.5 + children_pct, -0.5)* 0.4 + 0.0),
    x_b = list(c(0, 0.87, 0.87, 0, -0.87, -0.87) * 0.4 + 0.35),
    y_b = list(c(-1, -0.5, -0.5 + babies_pct, 0 + babies_pct, -0.5 + babies_pct, -0.5)* 0.4 - 0.2)
    ) %>% 
  unnest(cols = c(x, y, x_a, x_c, y_c, y_a, x_b, y_b))

city <- rooms %>%
  filter(hotel == "City Hotel") %>%
  ggplot() +
  #annotate("path", c(-0.87, 0, 0.87), c(-0.5, 0, -0.5), alpha = 0.1) +
  # "room"
  geom_polygon(aes(x, y), alpha = 1, fill = "grey85") +
  # adults
  geom_polygon(aes(x_a, y_a), alpha = 1, fill = "#6F8BA0") +
  # geom_text(aes(0, y_a, label = adults_sum), angle = 35, size = 1) +
  # children
  geom_polygon(aes(x_c, y_c), alpha = 1, fill = "#4A5F71") +
  # babies
  geom_polygon(aes(x_b, y_b), alpha = 1, fill = "#DFEBF1") +
  # geom_polygon(aes(x = x * 0.9, y = y * 0.9), alpha = 0.3) +
  coord_fixed() +
  theme_void(base_family = "JetBrains Mono Regular") +
  facet_wrap(vars(month), ncol = 6) +
  theme(
    plot.margin = margin(20, 20, 0, 20)
  )
  
resort <- rooms %>%
  filter(hotel == "Resort Hotel") %>%
  ggplot() +
  #annotate("path", c(-0.87, 0, 0.87), c(-0.5, 0, -0.5), alpha = 0.1) +
  geom_polygon(aes(x, y), alpha = 1, fill = "grey85") +
  geom_polygon(aes(x_a, y_a), alpha = 1, fill = "#6B8A8D") +
  geom_polygon(aes(x_c, y_c), alpha = 1, fill = "#4B5453") +
  geom_polygon(aes(x_b, y_b), alpha = 1, fill = "#ADD0B5") +
  # geom_polygon(aes(x = x * 0.9, y = y * 0.9), alpha = 0.3) +
  coord_fixed() +
  theme_void(base_family = "JetBrains Mono Regular") +
  facet_wrap(vars(month), ncol = 6) +
  theme(
    plot.margin = margin(0, 20, 20, 20)
  )
  
plot_grid(city, resort, labels = c("City Hotel", "Resort Hotel"), ncol = 1) +
  ggsave(here::here("2020-week07", "plots", "hotels-rooms.png"), dpi = 320,
         width = 12, height = 12)
                 
