library(tidyverse)
library(here)
library(fuzzyjoin)
library(ggimage)
library(ggrepel)

dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

dog_breeds <- tibble(filename = list.files(here("week-51", "png"))) %>% 
  mutate(
    breed_primary = str_to_title(
    str_replace_all(filename, c(".png" = "", "-" = " "))
    )
    )

child_f <-  dog_descriptions %>% 
  count(breed_primary, env_children) %>% 
  pivot_wider(names_from = env_children, names_prefix ="child_", values_from = n) %>% 
  mutate(
    child_total = child_FALSE + child_TRUE,
    child_friendliness = child_TRUE/(child_FALSE + child_TRUE),
  )

cat_f <-  dog_descriptions %>% 
  count(breed_primary, env_cats) %>% 
  pivot_wider(names_from = env_cats, names_prefix ="cat_", values_from = n) %>% 
  mutate(
    cat_total = cat_FALSE + cat_TRUE,
    cat_friendliness = cat_TRUE/(cat_FALSE + cat_TRUE),
  )

cc_f <- left_join(child_f, cat_f)

fuzzy_dogs <- stringdist_left_join(dog_breeds, cc_f) %>% 
  filter(!is.na(breed_primary.y)) %>% 
  filter(!is.na(child_friendliness)) %>% 
  filter(!is.na(cat_friendliness))

ggplot(fuzzy_dogs) +
  geom_point(aes(x = child_friendliness, y = cat_friendliness), size = 10, color = "#fff44f") +
  geom_image(aes(x = child_friendliness, y = cat_friendliness, image = paste0(here("week-51", "png"), "/", filename)), size = 0.05) +
  geom_label_repel(aes(x = child_friendliness, y = cat_friendliness, label = breed_primary.y), force = 6, point.padding = 3, box.padding = 1.2, segment.color = "black", segment.size = 0.2, fill = "black", color = "#fff44f", family = "IBM Plex Sans Bold", size = 4) +
  annotate("text", x = -0.05, y = 0.32, label = toupper("Child- and cat-friendliness\nof 30 dog breeds, as\nassessed* on Petfinder.com,\nfor adoptable dogs\nsince 2003"), hjust = 0, vjust = 1, lineheight = 0.9, size = 8, family = "IBM Plex Sans Bold") +
  annotate("text", x = -0.05, y = 0.12, label = toupper("*Based on presumed primary breed,\nas percentage of friendly to total number\nassessed for every breed"), hjust = 0, vjust = 1, lineheight = 0.9, size = 5, family = "IBM Plex Sans Bold") +
  
  labs(
    caption = "Source: Petfinder.com via The Pudding | Graphic: Georgios Karamanis\nIcons made by Freepik from www.flaticon.com",
    x = "← Less child-friendly  More child-friendly →",
    y = "← Less cat-friendly  More cat-friendly →"
  ) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "", "50%", "", "100%")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "", "50%", "", "100%")) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_family = "IBM Plex Mono Bold") +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 45, 20, 20),
    plot.background = element_rect(fill = "#fff44f", color = NA),
    panel.grid = element_line(color = "black", size = 0.05),
    axis.title = element_text(size = 16, color = "#b1a600"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    axis.text = element_text(size = 12, color = "#b1a600"),
    plot.caption = element_text(margin = margin(30, 0, 0, 0), color = "#b1a600")
  ) 

ggsave(
    here::here("week-51", "plots", "temp", paste0("dog-friendliness", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 12, width = 12
  )
 