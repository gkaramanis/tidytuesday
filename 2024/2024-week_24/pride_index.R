library(tidyverse)
library(ggcirclepack)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

pride_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')

comm_types <- c("large urban city",
                "medium city",
                "small city",
                "small town",
                "very small town",
                "rural community")

pride_it <- pride_index %>% 
  # fix a typo
  mutate(campus_location = str_replace(campus_location, "Swarrthmore", "Swarthmore")) %>% 
  mutate(community_type = fct_relevel(community_type, comm_types)) %>% 
  mutate(state = str_sub(campus_location, -2, -1)) %>% 
  mutate(campus_name = str_replace(campus_name, "University", "U"))

# Plot to get text labels
l <- ggplot(pride_it, aes(id = campus_name, area = students, fill = rating == 5)) +
  geom_circlepack(linewidth = 0.2) +
  geom_circlepack_text() +
  coord_fixed() +
  facet_wrap(vars(community_type))

# Get text labels and "add" community type for facets
cpl <- ggplot_build(l) %>%
  .$data %>%
  .[[2]] %>%
  mutate(
    community_type = comm_types[PANEL],
    community_type = fct_relevel(community_type, comm_types)
    ) %>% 
  group_by(community_type) %>% 
  mutate(min_y = min(y)) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot() +
  geom_text(data = cpl %>% distinct(community_type, min_y), aes(0, min_y -270, label = community_type), family = f1, fontface = "bold", size = 4, color = "#ED702D") +
  geom_segment(data = cpl %>% distinct(community_type, min_y), aes(x = 0, xend = 0, y = min_y, yend = min_y - 180), linewidth = 0.2, color = "#ED702D") +
  geom_circlepack(data = pride_it, aes(id = campus_name, area = students, fill = rating == 5), linewidth = 0.2, color = "grey99", linewidth = 0.1) +
  ggrepel::geom_text_repel(data = cpl %>% filter(fill == "#00BFC4"), aes(x, y, label = str_wrap(label, 20)), family = f1b, bg.color = alpha("grey99", 1), color = "grey10", size = 3, lineheight = 0.9, fontface = "bold", bg.r = 0.1, segment.color = "white") +
  scale_fill_manual(values = c("grey70", "#64157B")) +
  coord_fixed(clip = "off", expand = FALSE) +
  facet_wrap(vars(community_type), strip.position = "bottom") +
  labs(
    title = "The most LGBTQ-friendly campuses",
    subtitle = "Out of the 238 colleges and universities in the Campus Pride Index's database, 47 have a <span style='color:#64157B'>**top rating of 5**</span>.<br>Two-thirds of them are in small and medium-sized cities. Circle size represents the number of students.",
    caption = "Source: Campus Pride Index · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    strip.text = element_blank(),
    # strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = "#ED702D"),
    plot.subtitle = ggtext::element_markdown(size = 13, lineheight = 1.1, margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
