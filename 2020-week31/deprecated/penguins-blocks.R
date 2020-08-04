library(tidyverse)
library(ggforce)
library(colorspace)

penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

nm_suffix <- c("length",  "depth",   "flipper")

med <- penguins.csv %>% 
  filter(!is.na(bill_length_mm)) %>% 
  group_by(species) %>% 
  summarise(n = n(), across(matches("bill|flipper"), list(median = median, sd = sd), .names = "{fn}_{rep(nm_suffix, each = 2)}")) %>%
  group_by(species) %>% 
  mutate(species_n = 4 - cur_group_id())

white_grid <- tibble(x = seq(-40, 40, 10), y = 0, height = 80) 

ggplot(med) +
  # n = white part
  geom_rect(aes(
    xmin = 0 - 40, xmax = 0 + 40,
    ymin = 150, ymax = -150 + n * 1.4), size = 1, colour = "black") +
  # Bill 
  geom_rect(aes(
    xmin = 0 + 40, xmax = 0 + 40 + median_length,
    ymin = 130, ymax = 130 - median_depth), fill = "orange", colour = "black", size = 1) +
  # Flipper
  geom_rect(aes(
    xmin = 0 - 30, xmax = 0 + 10,
    ymin = 80, ymax = 80 - median_flipper), fill = "grey30", colour = "black", size = 1) +
  # Outline of penguin
  geom_tile(aes(x = 0, y = 0, width = 80, height = 300), fill = NA, colour = "black", size = 1) +
  # Grid
  geom_linerange(data = white_grid, aes(x, ))
  coord_fixed() +
  facet_wrap(vars(species)) +
  theme_void() +
  ggsave(here::here("2020-week31", "plots", "temp", paste0("penguin-blocks-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
