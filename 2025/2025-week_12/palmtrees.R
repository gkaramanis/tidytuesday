library(tidyverse)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')

palmtree_med <- palmtrees %>% 
  add_count(palm_tribe) %>% 
  filter(n >= 10) %>% 
  group_by(palm_tribe) %>%
  summarise(
    median_max_stem_height = round(median(max_stem_height_m, na.rm = TRUE), 1),
    median_max_leaf_number = round(median(max_leaf_number, na.rm = TRUE), 1),
    median_max_blade_length_m = round(median(max__blade__length_m, na.rm = TRUE), 1)
    ) %>%
  filter(!is.na(median_max_leaf_number)) %>% 
  filter(median_max_stem_height > 0) %>% 
  rowwise() %>% 
  mutate(leaf_angle = list(seq(-pi/3, pi + pi/3, length.out = median_max_leaf_number + 1))) %>%
  ungroup() %>% 
  unnest(leaf_angle) %>%
  mutate(
    x = 0,
    y = median_max_stem_height,
    xend = median_max_blade_length_m * cos(leaf_angle),
    yend = y + median_max_blade_length_m * sin(leaf_angle),
    palm_tribe = fct_reorder(palm_tribe, median_max_stem_height)
  )

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Young Serif"

ggplot(palmtree_med) +
  # Stem
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = y), stat = "unique") +
  # Leaves
  geom_curve(data = . %>% filter(xend > 0), aes(x = 0, y = y, xend = xend, yend = yend), curvature = -0.4, linewidth = 0.25) +
  geom_curve(data = . %>% filter(xend <= 0), aes(x = 0, y = y, xend = xend, yend = yend), curvature = 0.4, linewidth = 0.25) +
  # Stem height text
  geomtextpath::geom_textsegment(aes(x = -5, xend = -5, y = 0, yend = y, label = paste0(median_max_stem_height, " m")), linewidth = 0.2, stat = "unique", color = "coral4", family = f1, size = 5) +
  # Leaves length text
  geomtextpath::geom_textsegment(aes(x = 0, xend = median_max_blade_length_m, y = median_max_stem_height + median_max_blade_length_m + 1, yend = median_max_stem_height + median_max_blade_length_m + 1, label = paste0(median_max_blade_length_m, " m")), linewidth = 0.2, stat = "unique", size = 4, color = "darkgreen", family = f1) +
  # Leaves number text
  shadowtext::geom_shadowtext(aes(x = median_max_blade_length_m + 0.5, y = median_max_stem_height - median_max_blade_length_m/2, label = paste0(median_max_leaf_number)), stat = "unique", color = "purple4", bg.color = "white", family = f1, size = 5) +
  scale_linewidth_continuous(range = c(0.3, 2)) +
  scale_x_continuous(expand = c(0.2, 0.2)) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(palm_tribe), nrow = 3, strip.position = "bottom") +
  labs(
    title = "Palm tree characteristics by tribe",
    # Put colors in the subtitle with ggtext
    subtitle = "<span style='color:coral4'>median max **stem height**</span> · <span style='color:darkgreen'>median max **blade length**</span> · <span style='color:purple4'>median max **leaf number**</span>",
    caption = "Data: PalmTraits 1.0 database · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 24, face = "bold", margin = margin(35, 0, -35, 0), hjust = 0.5),
    plot.subtitle = element_markdown(size = 15, margin = margin(40, 0, -35, 0), hjust = 0.5),
    plot.caption = element_text(margin = margin(20, 0, 0, 0), hjust = 0.5),
    plot.margin = margin(-15, 10, 5, 15)
  )
