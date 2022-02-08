library(tidyverse)
library(camcorder)
library(ggimage)
library(ggtext)
library(grid)

gg_record(dir = "temp", device = "png", width = 14, height = 10, units = "in", dpi = 320)

# breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')

breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_top <- breed_rank %>% 
  janitor::clean_names() %>% 
  pivot_longer(x2013_rank:x2020_rank, names_to = "year", values_to = "rank") %>% 
  filter(rank < 10) %>% 
  mutate(year = parse_number(year)) %>% 
  group_by(year) %>% 
  arrange(rank) %>% 
  mutate(
    x = c(0.75, rep(seq(0.5, 3, length.out = 4), 2)),
    y = c(3, rep(c(1.5, 0), each = 4))
  )

f1 = "Porpora"
f2 = "Proxima Nova"
f3 = "Futura"

pal <- colorspace::darken(c("#3cb44b", "#4363d8", "#f58231", "#42d4f4", "#f032e6", "#469990", "#dcbeff", "#9A6324", "#800000", "#000075", "#a9a9a9", "#000000"), 0.3)

ggplot(breed_top) +
  # Rounded rect
  annotation_custom(grob = grid::roundrectGrob(gp = gpar(col = "grey70", fill = NA, lwd = 0.75)), xmin = -0.15, xmax = 3.65, ymin = -1.1, ymax = 3.5) +
  # No 1 image
  geom_image(data = breed_top %>% filter(rank == 1), aes(x, y, image = image), size = 0.35, asp = 1.2) +
  # No 2-9 images
  geom_image(data = breed_top %>% filter(rank > 1), aes(x, y, image = image), size = 0.22, asp = 1.2) +
  # Year
  geom_text(aes(2.5, 2.85, label = year), stat = "unique", family = f3, fontface = "bold", size = 16, color = "#13348E") +
  # Rank and breed text
  geom_textbox(aes(x + 0.1, ifelse(rank == 1, y - 0.75, y - 0.55), label = str_wrap(paste0("**", rank, "** ", breed), 15), color = breed), family = f2, size = 3, lineheight = 0.9, vjust = 1, width = 0.25, box.padding = unit(0, "mm"), box.color = NA) +
  # Stuff
  coord_fixed(xlim = c(0, 3.5), ylim = c(0, 3.5), expand = FALSE, clip = "off") +
  scale_color_manual(values = pal) +
  facet_wrap(vars(year), nrow = 2) +
  labs(
    title = toupper("Popularity of dog breeds by AKC registration statistics"),
    caption = "Source: American Kennel Club · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_blank(),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(6, "lines"),
    plot.title = element_text(size = 28, hjust = 0.5, family = f3, margin = margin(0, 0, 40, 0), face = "bold", color = "#13348E"),
    plot.caption = element_text(margin = margin(90, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )

