library(tidyverse)
library(geofacet)
library(ggh4x)
library(colorspace)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>% 
  mutate(animal_group_parent = str_to_sentence(animal_group_parent))

borough_names <- gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, name)

rescues_borough <- animal_rescues %>% 
  filter(cal_year < 2021) %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == "Cat", animal_group_parent, "Not_Cat")) %>% 
  count(cal_year, borough_code, animal_group_parent) %>% 
  filter(animal_group_parent == "Cat" | animal_group_parent == "Not_Cat") %>%
  pivot_wider(names_from = animal_group_parent, values_from = n) %>% 
  left_join(borough_names) %>% 
  filter(!is.na(name)) 

ggplot(rescues_borough, aes(x = cal_year)) +
  stat_difference(aes(ymin = Not_Cat, ymax = Cat), alpha = 0.3) +
  geom_line(aes(y = Not_Cat, color = "other")) +
  geom_line(aes(y = Cat, color = "cats")) +
  scale_x_continuous(breaks = seq(2010, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 20, 10)) +
  scale_color_manual(values = c("#3D85F7", "#C32E5A")) +
  scale_fill_manual(values = c(lighten("#3D85F7"), lighten("#C32E5A"), "grey60"), labels = c("more cats", "more other", "same")) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  facet_geo(vars(name), grid = "gb_london_boroughs_grid") +
  labs(
    title = "Rescues of\ncats vs other animals by\nthe London fire brigade\n2009-2020",
    caption = "Source: London.gov · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Fira Sans Compressed") +
  theme(
    legend.position = c(0.875, 0.975),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#F5F4EF", color = NA),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, family = "KyivType Sans", face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave(here::here("temp", paste0("animal-rescues-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 10)
