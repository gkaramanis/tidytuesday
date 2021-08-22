library(here)
library(tidyverse)
library(gridExtra)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

# https://www.massaudubon.org/learn/nature-wildlife/birds/commonly-confused-birds
# Hairy Woodpecker & Downy Woodpecker
# Purple Finch & House Finch
# Chipping Sparrow, American Tree Sparrow, & House Sparrow
# Sharp-Shinned Hawk & Cooper's Hawk

confused <- bird_counts %>%
  mutate(
    pair = case_when(
      species == "Hairy Woodpecker" | 
        species == "Downy Woodpecker" ~ "A",
      species == "American Tree Sparrow" | 
        species == "House Sparrow" ~ "B",
      species == "Sharp-shinned Hawk" | 
        species == "Cooper's Hawk" ~ "C",
                     TRUE ~ "")
    ) %>%
  filter(pair != 0)

confusedSplit <- split(confused, f = confused$pair)

p1 <- ggplot(confusedSplit$A, aes(year, how_many_counted, group = species)) +
  geom_area(aes(fill = species), alpha = 0.9) +
  labs(title = "What did I just see?",
       subtitle = "Observations of three pairs of commonly confused bird species\nat the Christmas Bird Counts in Hamilton, Ontario, between 1921 and 2017") +
  scale_y_continuous(position = "right", breaks = c(0, 400)) +
  scale_fill_manual(values = c("#0072cf", "#ffe71a")) +
  theme(
    plot.background = element_rect(fill = "grey90", color = "grey90"),
    panel.background = element_rect(fill = "grey90"),
    plot.title = element_text(family = "IBM Plex Sans Bold"),
    plot.subtitle = element_text(margin = margin(b = 1, unit = "cm")),
    legend.background = element_rect(fill = "grey90"),
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "line"),
    legend.key = element_rect(fill = "grey90", color = "grey90"),
    legend.title = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x = element_line(size = rel(0.4)),
    axis.title = element_blank(),
    axis.text  = element_text(color = "grey50"),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(5, "points"),
    text = element_text(family = "IBM Plex Sans"),
    panel.spacing = unit(3, "lines"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )
  
p2 <- ggplot(confusedSplit$B, aes(year, how_many_counted, group = species)) +
  geom_area(aes(fill = species), alpha = 0.9) +
  scale_y_continuous(position = "right", breaks = c(0, 4000)) +
  scale_fill_manual(values = c("#7a2531", "#fdd475")) +
  theme(
    plot.background = element_rect(fill = "grey90", color = "grey90"),
    panel.background = element_rect(fill = "grey90"),
    legend.background = element_rect(fill = "grey90"),
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "line"),
    legend.key = element_rect(fill = "grey90", color = "grey90"),
    legend.title = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x = element_line(size = rel(0.4)),
    axis.title = element_blank(),
    axis.text  = element_text(color = "grey50"),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(5, "points"),
    text = element_text(family = "IBM Plex Sans"),
    panel.spacing = unit(3, "lines"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )

p3 <- ggplot(confusedSplit$C, aes(year, how_many_counted, group = species)) +
  geom_area(aes(fill = species), alpha = 0.9) +
  labs(caption = "Source: Bird Studies Canada | Graphic: Georgios Karamanis") +
  scale_y_continuous(position = "right", breaks = c(0, 30)) +
  scale_fill_manual(values = c("#485dc5", "#e99fdb")) +
  theme(
    plot.background = element_rect(fill = "grey90", color = "grey90"),
    panel.background = element_rect(fill = "grey90"),
    plot.caption = element_text(margin = margin(t = 1, unit = "cm")),
    legend.background = element_rect(fill = "grey90"),
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "line"),
    legend.key = element_rect(fill = "grey90", color = "grey90"),
    legend.title = element_blank(), 
    panel.grid = element_blank(),
    axis.line.x = element_line(size = rel(0.4)),
    axis.title = element_blank(),
    axis.text  = element_text(color = "grey50"),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(5, "points"),
    text = element_text(family = "IBM Plex Sans"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )

# grid.arrange(p1, p2, p3, heights = c(3, 2, 3))

g <- arrangeGrob(p1, p2, p3, heights = c(4.5, 3, 3.5))

ggsave(here("week-25", "xBirdCounts.png"), g, height = 11, width = 7, dpi = 300)

