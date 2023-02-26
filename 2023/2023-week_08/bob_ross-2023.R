library(tidyverse)
library(patchwork)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10.5, height = 8, units = "in", dpi = 320)

bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
  
f1 <- "Outfit"

bob_polar <- bob_ross %>% 
  # filter(season < 10) %>% 
  separate_longer_delim(color_hex, ", ") %>%
  mutate(color_hex = str_remove_all(color_hex, "\\[|\\]|'")) %>% 
  group_by(season, episode) %>% 
  mutate(col_id = row_number()) %>% 
  ungroup() %>% 
  add_row(season = seq(0, 0.9, length.out = 4)) 

p <- ggplot(bob_polar) +
  # Episode numbering
  geom_tile(aes(episode, col_id, fill = color_hex), color = NA) +
  geom_text(data = . %>% filter(episode %% 2 != 0), aes(episode, num_colors + 3, label = episode), stat = "unique", size = 2, family = f1, color = "grey50") +
  geom_text(data = . %>% filter(episode %% 2 == 0), aes(episode, num_colors + 3, label = "·"), stat = "unique", size = 3, family = f1, color = "grey50") +
  scale_y_continuous(limits = c(-2, 16)) +
  scale_fill_identity() +
  coord_polar() +
  facet_wrap(vars(season), ncol = 7, labeller = labeller(season = function(x) {ifelse(x >= 1, paste("Season", x), " ")})) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey94", color = NA),
    strip.text = element_text(face = "bold", color = "#0A3410"),
    plot.margin = margin(5, 0, 0, 0)
  )

annot <- ggplot() +
  geom_text(aes(0, 0, label = "The Colors of Bob Ross"), family = f1, fontface = "bold", size = 12, color = "#102E3C") +
  geom_richtext(aes(0, -1, label = "<span style='color:#102E3C;font-size:20px'>**Colors used for each painting/episode**</span><br>Source: Jared Wilber · Graphic: Georgios Karamanis"), vjust = 1, family = f1, size = 4, color = "#C79B00", fill = NA, label.color = NA, lineheight = 1.5) +
  scale_y_continuous(limits = c(-1.2, 0)) +
  theme_void() +
  coord_cartesian(clip = "off")

p +
  inset_element(annot, 0, 0.9, 0.6, 0.96) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "grey94", color = NA))
    )
