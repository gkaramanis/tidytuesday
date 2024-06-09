library(tidyverse)
library(ggforce)  
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

cheese_colors <- tribble(
  ~color,           ~hex,
  "yellow",         "#FFFF00",
  "ivory",          "#FFFFF0",
  "white",          "#FFFFFF",
  "pale yellow",    "#FFFF99",
  "blue",           "#0000FF",
  "orange",         "#FFA500",
  "cream",          "#FFFDD0",
  "brown",          "#A52A2A",
  "green",          "#008000",
  "golden yellow",  "#FFDF00",
  "pale white",     "#F5F5F5",
  "straw",          "#E4D96F",
  "brownish yellow","#CC9966",
  "blue-grey",      "#6699CC",
  "golden orange",  "#FFA500",
  "red",            "#FF0000",
  "pink and white", "#FFC0CB",
  NA,               "grey80"
)

countries <- c("Italy", "France", "United States")

plot_cheeses <- cheeses %>% 
  filter(country %in% countries) %>%
  count(country, color) %>% 
  group_by(country) %>% 
  mutate(color = fct_lump(color, w = n, n = 5, other_level = "other")) %>%
  ungroup() %>% 
  group_by(country, color) %>% 
  summarise(n = sum(n)) %>% 
  mutate(share = n / sum(n)) %>% 
  left_join(cheese_colors) %>% 
  mutate(
    end = cumsum(share) * 2*pi,
    start = lag(end, default = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    cf = case_match(
      country,
      "Italy" ~ 2, 
      "France" ~ 4, 
      "United States" ~ 6
    )
  )

p <- ggplot(plot_cheeses) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = cf - 0.7, r = cf + 1, start = start + 0.01, end = end - 0.01, fill = hex)) +
  scale_fill_identity() +
  coord_fixed() 

col_labs <- ggplot_build(p)$data %>% 
  .[[1]] %>% 
  group_by(group) %>% 
  mutate(
    midx = mean(x),
    midy = mean(y)
  ) %>% 
  ungroup() %>% 
  distinct(fill, midx, midy) %>% 
  left_join(plot_cheeses %>% distinct(color, hex), by = c("fill" = "hex")) %>% 
  mutate(color = replace_na(color, "missing"))

annot <- data.frame(
  x = c(1.8, 4.1, 6.3),
  y = c(-1, -1, -1),
  label = countries
)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

bg_col <- "#F4F4F4"
line_col <- "grey20"

ggplot() +
  # Countries
  ggrepel::geom_label_repel(data = annot, aes(x, y, label = label), direction = "y", nudge_y = -7, stat = "unique",  family = f2, label.padding = 0.5, label.size = 0, fill = bg_col, fontface = "bold", segment.color = line_col, color = "black", box.padding = 0) +
  # Arc bars
  geom_arc_bar(data = plot_cheeses, aes(x0 = 0, y0 = 0, r0 = cf - 0.7, r = cf + 1, start = start + 0.01, end = end - 0.01, fill = hex, color = !is.na(color)), radius = unit(0.5, 'mm'), linewidth = 0.5) +
  # Colors
  ggrepel::geom_text_repel(data = col_labs, aes(midx, midy, label = color), , family = f1b, fontface = "bold", bg.color = "grey10", color = "white") +
  scale_color_manual(values = c("white", line_col)) +
  scale_fill_identity() +
  labs(
    title = "Cheese colors",
    subtitle = "Colors of the cheeses from the top 3 countries with the most cheeses in the cheese.com database",
    caption = "Source: cheese.com · Graphic: Georgios Karamanis"
  ) +
  coord_fixed() +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(10, 10, 10, 0),
    plot.title = element_text(family = f2, face = "bold", size = 20)
  )

