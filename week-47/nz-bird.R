library(tidyverse)
library(here)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

all_votes <- nz_bird %>% 
  filter(!is.na(bird_breed)) %>% 
  group_by(bird_breed) %>%
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  mutate(bird_breed = fct_reorder(bird_breed, n))

all_votes %>% 
ggplot() + 
  geom_bar(aes(bird_breed, fill = vote_rank), position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#D50000", "grey45", "grey40", "grey35", "grey30")) +
  scale_y_continuous(breaks = seq(0, 12500, 2000)) +
  coord_flip(expand = FALSE) +
  labs(
    title = "New Zealand Bird of the Year 2019",
    subtitle = "Number of votes for every bird, by order of preference - first (red), second, third, fourth and fifth",
    caption = "Source: New Zealand Forest and Bird Orginization | Graphic: Georgios Karamanis"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA),
    axis.text.y = element_text(family = "IBM Plex Sans", size = 5, color = "grey95"),
    axis.text.x = element_text(family = "IBM Plex Mono", size = 6, color = "white"),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey30", size = 0.3),
    panel.grid.minor.x = element_line(color = "grey20"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 11, color = "white", margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_text(family = "IBM Plex Sans", size = 8, color = "white", margin = margin(2, 0, 10, 0)),
    plot.caption = element_text(family = "IBM Plex Sans", size = 5, color = "grey50", margin = margin(10, 0, 0, 0))
    ) 

ggsave(
      here::here("week-47", "plots", "temp", paste0("nzbird-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 9
      )
 