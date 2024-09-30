library(tidyverse)
library(ggsankey)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')

# Make moves wide
chess_moves <- chess %>% 
  select(game_id, moves) %>% 
  mutate(moves = str_split(moves, " ")) %>% 
  rename(move = moves) %>% 
  unnest_wider(move, names_sep = "_")

# Create a long format dataset for ggsankey
chess_moves_sankey <- chess_moves %>% 
  # head(500) %>% # Uncomment to use a subset of the data, for speed
  make_long(2:6) 

# Create an initial Sankey diagram (not used in final plot, but used to extract data)
p <- ggplot(chess_moves_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, label = node)) +
  geom_sankey(flow.alpha = 0.6) +
  geom_sankey_text() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

# Extract flow, node, and label data from the Sankey diagram
moves_flows <- layer_data(p, 1) %>% 
  mutate(height = flow_end_ymax - flow_end_ymin)

moves_nodes <- layer_data(p, 2)

moves_labels <- layer_data(p, 3)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot() +
  # Add flow polygons, highlighting the most common moves
  geom_polygon(data = moves_flows, aes(x, y, group = group, fill = height > max(height) * 0.1)) +
  # Add node rectangles
  geom_rect(data = moves_nodes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "black") +
  # Add labels for the most common moves
  geom_text(data = moves_labels %>% filter(freq > max(freq) * 0.1), aes(x, y, label = label), family = f1b, color = "white", size = 3.5) +
  scale_fill_manual(values = c("white", "black")) +
  labs(
    title = "First Five Moves",
    subtitle = "Flow of opening moves in over 20\u00A0000 chess games from Lichess.org. The most popular moves are\nhighlighted in black, revealing common opening strategies.",
    caption = "Source: Lichess.org via Kaggle/Mitchell J. · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#eedc97", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 30, face = "bold", family = f2),
    plot.subtitle = element_text(size = 15, lineheight = 1),
    plot.caption = element_text(hjust = 0, size = 11)
  )