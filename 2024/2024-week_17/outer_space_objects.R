library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

outer_space_objects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv') %>% 
  janitor::clean_names()

outer_decade <- outer_space_objects %>% 
  filter(entity != "World") %>% 
  mutate(decade = year %/% 10 * 10) %>% 
  group_by(entity) %>% 
  mutate(
    total = sum(num_objects, na.rm = TRUE),
    entity_label = paste0(entity, " **", scales::number(total), "**"),
    color = ifelse(total > 100, entity, NA)
    ) %>% 
  ungroup()

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Bricolage Grotesque 12pt Condensed"

pal <- c("#0C2D57", "#FC6736", "#FFB0B0", "#EFECEC")

annot <- outer_decade %>% 
  filter(entity == "United States" & decade == 2020)

set.seed(9)

ggplot(outer_decade) +
  # annotation
  ggforce::geom_mark_circle(data = annot, aes(entity_label, decade, description = "The United States has launched 6 321, or two thirds, of its 9 632 objects in just the past four years"), con.cap = 0, label.family = f1, label.buffer = unit(22, "lines"), con.colour = alpha(pal[3], 0.5), colour = alpha(pal[3], 0.7), label.fill =  alpha(pal[4], 1)) +
  geom_hline(aes(yintercept = decade + 5), color = pal[3]) +
  # space objects
  geom_text(aes(entity_label, decade, color = color, label = "⨯", size = num_objects), position = position_jitter(height = 3, width = 0.5), angle = runif(1108, 0, 90), alpha = 0.8, key_glyph = draw_key_point) +
  # entities
  ggtext::geom_richtext(aes(entity_label, 2027, label = entity_label, color = color), angle = 90, family = f1, hjust = 0, stat = "unique", fill = NA, label.size = 0) +
  # coord, scales and theme
  coord_radial(start = -pi/1.6, end = pi/1.6, inner.radius = 0.1, expand = FALSE, rotate_angle = TRUE, clip = "off") +
  scale_y_continuous(breaks = unique(outer_decade$decade), sec.axis = dup_axis()) +
  scale_color_manual(values = rep(pal[4], 10), na.value = pal[2]) +
  scale_size_continuous(range = c(5, 20), breaks = c(1, 100, 1000, 2000)) +
  guides(
    theta = guide_axis_theta(angle = 90),
    color = "none",
    size = guide_legend(nrow = 1, override.aes = list(shape = "⨯", color = pal[2]))
    ) +
  labs(
    title = "Objects launched into outer space",
    subtitle = str_wrap("Satellites, probes, landers, crewed spacecraft, and space station flight elements launched into Earth orbit or beyond. The numbers show the total objects launched by each country or organization from 1957 to 2023. The size of each data point corresponds to the number of objects launched by an entity in a particular year. Entities that have launched more than 100 objects in total are highlighted in white.", 136),
    caption = "Source: UN Office for Outer Space Affairs · Graphic: Georgios Karamanis",
    size = "Number of objects"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "none",
    legend.title = element_text(color = pal[2], hjust = 0.5, margin = margin(0, 0, -10, 0)),
    legend.title.position = "top",
    legend.text = element_text(color = pal[2], vjust = 0.33, margin = margin(0, 0, 0, -5)),
    plot.background = element_rect(fill = pal[1], color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = pal[3]),
    plot.margin = margin(10, 10, 10, 0),
    plot.title = element_text(color = pal[3], size = 26, face = "bold"),
    plot.subtitle = element_text(color = pal[3], size = 14),
    plot.caption = element_text(color = pal[3], hjust = 0.5, size = 12)
  )
