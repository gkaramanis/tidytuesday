library(tidyverse)
library(widyr)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 7, units = "in", dpi = 320)

parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv') %>% 
  janitor::clean_names() 

# Calculate pairwise correlations between main accords (using Phi Coefficient)
pairwise_cors <- parfumo_data_clean %>%
  filter(main_accords != "Main Accords") %>% # clean data
  separate_rows(main_accords, sep = ",") %>%
  mutate(main_accords = str_trim(main_accords)) %>% #remove extra spaces
  pairwise_cor(feature = name, item = main_accords, sort = TRUE) %>% 
  group_by(item1) %>% 
  arrange(desc(correlation)) %>%  # Sort by correlation descending
  mutate(i = if_else(
    correlation >= 0,
    0.2 + row_number(),  # Count up from 1 for strongest positive
    -1.2 -(sum(correlation < 0) - (row_number() - sum(correlation >= 0)))  # Count up from max negative
  )) %>%
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(pairwise_cors) +
  geom_text(aes(item1, 0, label = item1), size = 3, fontface = "bold", family = f1b) +
  geom_text(aes(item1, i, label = item2, size = abs(correlation), color = correlation, fontface = if_else(abs(correlation) > 0.2, "bold", "plain")), family = f1b) +
  scale_size_continuous(range = c(1, 3)) +
  MetBrewer::scale_color_met_c("Paquin") +
  coord_radial(rotate.angle = TRUE, inner.radius = 0.1) +
  labs(
    title = "Fragrance accord correlations",
    subtitle = "Positive and negative correlations between main accords in perfumes\nBolder and larger text indicates stronger relationships",
    caption = "Data: Parfumo.net · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F4F1ED", color = NA),
    plot.title = element_text(family = f2, size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(5, 0, -60, 0)),
    plot.caption = element_text(size = 8, hjust = 0.5, margin = margin(-50, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  ) 
  
