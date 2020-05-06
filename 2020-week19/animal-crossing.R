library(tidyverse)
library(ggimage)
library(ggforce)

villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

village_people <- villagers %>% 
  add_count(species, personality) %>% 
  group_by(personality) %>% 
  arrange(species) %>% 
  mutate(
    personality_x = row_number()
    ) %>% 
  ungroup()

# make dummy df for annotating the caption
dummy <- data.frame(x = c(1, 5), y = c(0))
# make grob for custom annotation of caption
g <- ggplotGrob(ggplot(dummy, aes(x, y)) + 
                  geom_point(colour = NA) +
                  geom_mark_ellipse(fill = "#E3C089", colour = NA, alpha = 1) +
                  coord_fixed(clip = "off") +
                  theme_void())

ggplot(village_people) +
  annotation_custom(g, xmin = 33.7, xmax = 44.5, ymin = 8.74) +
  geom_mark_ellipse(aes(personality_x, personality, filter = n > 4, fill = species), colour = NA, alpha = 1, position = position_nudge(y = -0.2), expand = unit(1.4, "mm")) +
  # geom_point(aes(personality_x, personality_y)) +
  geom_image(aes(personality_x, personality, image = url), asp = 2, size = 0.05, by = "height") +
  coord_fixed(ratio = 4, clip = "off") +
  scale_fill_manual(values = c(rep("#E3C089", 7))) +
  labs(
    title = "Lazy dogs and peppy rabbits",
    subtitle = "Villager personalities and species.  Common combinations",
    caption = "Source: VillagerDB | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#61A554", colour = "#61A554"),
    plot.margin = margin(20, 30, 20, 30),
    axis.text.y = element_text(colour = "#FEFAE6", family = "Arial Rounded MT Bold", hjust = 1),
    plot.title = element_text(colour = "#F8D066", size = 28, family = "FinkHeavy", hjust = 0.5),
    plot.subtitle = element_text(colour = "white", size = 14, family = "Arial Rounded MT Bold", hjust = 0.5, margin = margin(15, 0, 0, 0)),
    plot.caption = element_text(colour = "#F8D066", size = 9, family = "FinkHeavy", hjust = 0.5)
  ) +
  ggsave(here::here("2020-week19", "plots", "temp", paste0("animal-crossing-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 8)
