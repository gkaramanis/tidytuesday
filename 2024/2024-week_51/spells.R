library(tidyverse)
library(ggcirclepack)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8.5, units = "in", dpi = 320)

spells <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')

spells_pack <- spells %>%
  mutate(duration = case_when(
    duration == "Concentration up to 10 minutes" ~ "Concentration, up to 10 minutes",
    TRUE ~ duration
  )) %>% 
  pivot_longer(bard:wizard, names_to = "class") %>% 
  filter(value) %>% 
  mutate(level = factor(level)) %>% 
  arrange(class, level) %>% 
  add_count(school, class)

f1 <- "Graphik"
f2 <- "Copperplate"

pal <- MetBrewer::met.brewer("Greek", 10, direction = -1)

ggplot(spells_pack) +
  geom_text(aes(x = -9, y = -9, label = n), stat = "unique", hjust = 0, vjust = 0, size = 7, color = "#2C3E50", alpha = 0.1, family = f1, fontface = "bold") +
  geom_circlepack(aes(id = name, fill = level, area = as.numeric(level))) +
  scale_fill_manual(values = pal) +
  coord_fixed(clip = "off") +
  facet_grid(vars(school), vars(class)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Dungeons and Dragons Free Rules (2024 edition)",
    subtitle = "Spells by school of magic and class. Circle size and color show spell level",
    caption = "Source: D&D Beyond · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f2) +
  theme(
    legend.position = "top",
    legend.text.position = "bottom",
    legend.title = element_text(vjust = 0.75),
    legend.background = element_rect(fill = "#EBE2D1"),
    legend.key = element_blank(),
    plot.background = element_rect(fill = "#EBE2D1", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text.y = element_text(size = 7),
    strip.background = element_rect(fill = "#2C1810", color = "#4A2511"),
    strip.text = element_text(color = "white"),
    plot.title = element_text(family = f2, face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(family = f2, hjust = 0.5, size = 12),
    plot.caption = element_text(family = f2, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
  
