library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

drwho_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')

drwho_directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_directors.csv') %>%
  # Add non-breaking space to Jamie Magnus Stone (used later in the plot)
  mutate(director = case_when(
    str_detect(director, "Jamie Magnus") ~ str_replace(director, "Jamie Magnus", "Jamie\u00A0Magnus"),
    TRUE ~ director
  ))

drwho_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

drwho <- drwho_directors %>% 
  left_join(drwho_writers) %>% 
  left_join(drwho_episodes) %>% 
  select(director, writer, story_number) %>% 
  mutate(writer = strsplit(writer, " and ")) %>% 
  unnest(writer)

drwho_combo <- drwho %>% 
  count(director, writer, sort = TRUE) %>% 
  filter(n > 2)

drwho_graph <- drwho %>% 
  filter(director %in% drwho_combo$director) %>% 
  filter(writer %in% drwho_combo$writer) %>% 
  group_by(director, writer) %>% 
  mutate(pair = cur_group_id()) %>% 
  ungroup() %>% 
  as_tbl_graph() 

f1 <- "Sharpie"
f2 <- "Movement"
f3 <- "Outfit"

pal <- MetBrewer::met.brewer("Cross", n = nrow(drwho_combo))

ggraph(drwho_graph, layout = "fabric") + 
  geom_hline(yintercept = 4:17, color = pal[1:14], linewidth = 0.1) +
  geom_node_range(linetype = "dotted", color = "grey80", linewidth = 0.7) +
  geom_edge_span(aes(colour = pair), end_shape = "square", edge_width = 0.8) +
  # Directors
  geom_node_text(data = . %>% filter(name %in% drwho_combo$director), aes(x = xmin, label = str_wrap(name, 7)), hjust = 0, vjust = 0, nudge_x = -0.2, nudge_y = 0.3, family = f2, fontface = "bold", size = 4, color = "purple4", lineheight = 0.8) +
  # Writers
  geom_node_text(data = . %>% filter(name %in% drwho_combo$writer), aes(x = xmin, label = str_wrap(toupper(name), 10)), hjust = 1, nudge_x = -1.2, family = f1, fontface = "bold", size = 4.5, color = "orange4", lineheight = 0.8) +
  scale_edge_color_gradientn(colors = pal) +
  coord_cartesian(clip = "off") +
  labs(
    title = "DOCTOR WHO",
    subtitle = "Pairs of <span style='color:purple4; font-family:Movement'>directors</span> and <span style='color:orange4; font-family:Sharpie'>WRITERS</span><br>that have collaborated with<br>each other 3 times or more<br>(2005–2022)",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f3) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F5F8FB", color = NA),
    plot.margin = margin(10, 30, 10, 40),
    plot.title = shadowtext::element_shadowtext(face = "bold", size = 40, hjust = 1, margin = margin(10, 0, 30, 0), color = "white"),
    plot.subtitle = element_markdown(face = "bold", size = 19, hjust = 1, lineheight = 1.2, margin = margin(5, 0, -95, 0), color = "#00203c"),
    plot.caption = element_text(hjust = 0, face = "bold", color = "#00203c")
  )
