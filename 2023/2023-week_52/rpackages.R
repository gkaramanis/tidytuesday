library(tidyverse)
library(ggraph)
library(tidygraph)
library(ggtext)
library(grid)
library(ggpp)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Read in data
cran_20221122 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-26/cran_20221122.csv')

external_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-26/external_calls.csv')


gg_packages <- external_calls %>% 
  filter(str_detect(package_to, "ggplot2")) %>% # packages that make a call to ggplot2
  left_join(cran_20221122, by = c("package_from" = "package")) %>% 
  select(from = package_from, to = depends) %>% # keep package name and dependencies
  filter(!is.na(to)) 

# Create tbl_graph object
g <- as_tbl_graph(gg_packages, directed = TRUE) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_pagerank()) 

# Fonts
f1 <- "Outfit"
f2 <- "Founders Grotesk"

# Colors
pal <- MetBrewer::met.brewer("Klimt")

col_yellow <- colorspace::lighten(pal[3], 0.9)
col_green <- pal[4]
col_blue <- pal[5]
col_purple <- pal[6]

# Create circle with gradient fill
circle_gr <- grid.circle(gp = gpar(col = col_blue, fill = radialGradient(cy1 = 0.85, colours = c(col_yellow, "white"))))

circle <- data.frame(x = 1.1, y = -4.2) %>% 
  mutate(grob = list(circle_gr))

# Network graph
p <- ggraph(g, layout = "kk") +
  geom_grob(data = circle, aes(x, y, label = grob), vp.height = 0.95, vp.width = 0.95) +
  geom_node_point(size = 1, color = col_green) +
  geom_edge_link(aes(edge_width = after_stat(index), color = after_stat(index)), lineend = "round") +
  # Label only the "biggest" nodes
  geom_node_text(data = . %>% filter(round(degree, 2) > 0), aes(label = name), size = 6, family = f1, fontface = "bold") +
  scale_edge_width_continuous(range = c(1.7, 0.1)) +
  scale_edge_color_gradient(high = col_yellow, low = col_purple) 

# Get labels for nodes
p_labels <- ggplot_build(p) %>% 
  .$data %>% 
  .[[4]]

# Plot network graph with geom_shadowtext() for the labels
p +
  shadowtext::geom_shadowtext(data = p_labels, aes(x, y, label = label), family = f1, size = 6, fontface = "bold", color = col_green, bg.color = col_yellow) +
  coord_fixed() +
  labs(
    title = "<span style='color:#469d76'>Dependencies</span> of all <span style='color:#924099'>R packages</span> that make a call to ggplot2",
    caption = "Source: Mark Padgham & Noam Ross · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_yellow, color = NA),
    plot.title = element_markdown(hjust = 0.5, size = 24, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 13, color = col_blue, face = "bold"),
    plot.margin = margin(10, 0, 10, 0)
  )