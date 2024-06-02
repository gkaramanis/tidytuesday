library(tidyverse)
library(tidygraph)
library(ggraph)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.5, height = 8, units = "in", dpi = 320)

harvest_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2020.csv')
harvest_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2021.csv')

categories <- tribble(
  ~category,         ~vegetable,
  "leafy greens",      "lettuce",
  "leafy greens",      "spinach",
  "leafy greens",         "kale",
  "leafy greens",  "swiss chard",
  "leafy greens",      "cabbage",
  "root vegetables",       "radish",
  "root vegetables",        "beets",
  "root vegetables",      "carrots",
  "root vegetables",     "potatoes",
  "root vegetables", "sweet potato",
  "root vegetables",     "rutabaga",
  "root vegetables",       "garlic",
  "root vegetables",       "onions",
  "fruits",     "tomatoes",
  "fruits",      "peppers",
  "fruits",    "jalapeño",
  "fruits",  "hot peppers",
  "fruits",    "cucumbers",
  "fruits",     "zucchini",
  "fruits",     "pumpkins",
  "fruits",       "squash",
  "fruits",   "watermelon",
  "fruits",   "tomatillos",
  "berries", "strawberries",
  "berries",  "raspberries",
  "legumes",         "peas",
  "legumes",        "beans",
  "legumes",      "edamame",
  "herbs",       "chives",
  "herbs",     "cilantro",
  "herbs",        "basil",
  "herbs",         "mint",
  "herbs",      "oregano",
  "herbs",         "dill",
  "cruciferous vegetables",     "broccoli",
  "cruciferous vegetables",     "kohlrabi",
  "other",    "asparagus",
  "other",         "corn",
  "other",       "apples"
)


all_harvest <- bind_rows(harvest_2020, harvest_2021) %>% 
  mutate(
    vegetable = tolower(vegetable),
    vegetable = case_when(
      vegetable == "apple" ~ "apples",
      vegetable == "pumpkin" ~ "pumpkins",
      TRUE ~ vegetable
    )
  ) %>% 
  distinct(vegetable, variety) %>% 
  left_join(categories) %>% 
  select(category, vegetable, variety) %>% 
  mutate(name = "Lisa's garden", .before = 1)

edges_level0_1 <- all_harvest %>% 
  distinct(from = name, to = category) 

edges_level1_2 <- all_harvest %>% 
  distinct(from = category, to = vegetable) 

edges_level2_3 <- all_harvest %>% 
  distinct(from = vegetable, to = variety) 

edge_list <- rbind(edges_level0_1, edges_level1_2, edges_level2_3)

harvest_graph <- as_tbl_graph(edge_list)

p <- ggraph(harvest_graph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  geom_node_text(aes(label = name)) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

node_labels <- ggplot_build(p) %>% 
  .$data %>% 
  .[[3]] %>% 
  select(1:3) %>% 
  mutate(
    vegetable = if_else(label %in% all_harvest$vegetable, TRUE, FALSE),
    category = if_else(label %in% all_harvest$category, TRUE, FALSE),
    variety = if_else(label %in% all_harvest$variety, TRUE, FALSE)
  )

harvest_points <- ggplot_build(p) %>% 
  .$data %>% 
  .[[1]] %>% 
  filter(group > 8)

f1 <- "Graphik"
f1b <- "Graphik Compact"

pal <- c(
  "darkgreen",
  "#c6c736",
  "#96b125",
  "#fea938",
  "#fcec9a"
  )
  

ggplot(harvest_points, aes(x, y)) +
  geom_path(aes(group = group), color = pal[3], size = 0.3) +
  # varieties
  shadowtext::geom_shadowtext(data = node_labels %>% filter(variety), aes(x, y, label = label, size = if_else(vegetable|category, 3.5, 3), fontface = if_else(vegetable|category, "bold", "plain")), family = f1b, angle = 90, hjust = 0, nudge_y = 0.05, bg.color = pal[5], color = pal[1]) +
  # vegetables
  # shadowtext::geom_shadowtext(data = node_labels %>% filter(vegetable & !variety), aes(x, y, label = label), family = f1b, angle = 90, hjust = 0.8, fontface = "bold", size = 3.5, bg.color = pal[1]) +
  shadowtext::geom_shadowtext(data = node_labels %>% filter(vegetable & !variety), aes(x, y, label = label), family = f1b, angle = 90, hjust = 0.8, fontface = "bold", size = 3.5, color = pal[1], bg.color = pal[5]) +
  # categories
  geom_text(data = node_labels %>% filter(category), aes(x, y, label = str_wrap(label, 10)), family = f1b, angle = 90, hjust = 1, nudge_y = -0.1, fontface = "bold", lineheight = 0.75, color = pal[4]) +
  scale_y_reverse() +
  scale_size_identity() +
  coord_radial(rotate.angle = TRUE, inner.radius = 0.3, clip = "off", end = 1.85 * pi, start = 0.15 * pi) +
  labs(
    title = "Lisa's garden",
    subtitle = "Source: {gardenR} · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = pal[5], color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, margin = margin(50, 0, -100, 0), face = "bold", size = 30, lineheight = 0.85, color = pal[1]),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(110, 0, -110, 0), size = 9, lineheight = 1.1, color = pal[3], face = "bold")
  )
