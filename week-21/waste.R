library(tidyverse)
library(ggimage)
library(janitor)

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_vs_gdp <- clean_names(mismanaged_vs_gdp)

mismanaged_vs_gdp %>%
  rename(percapita_mismanaged_kg_pp_pd = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day) %>% 
  filter(entity != "World" & year == 2010) %>% 
  top_n(30, percapita_mismanaged_kg_pp_pd) %>% 
  mutate(bar = map2(0.01, percapita_mismanaged_kg_pp_pd, seq, by = 0.01)) %>% 
  unnest(bar) %>% 
  mutate(plastic = sample(c("https://cdn1.iconfinder.com/data/icons/fitness-icon-collection/100/plastic-128.png",
                            "https://image.flaticon.com/icons/png/128/81/81940.png",
                            "https://image.flaticon.com/icons/png/128/1758/1758890.png",
                            "https://image.flaticon.com/icons/png/128/960/960773.png"),
                          size =  nrow(.), replace = TRUE)) %>% 
  ggplot(aes(fct_rev(factor(entity)), bar)) +
  geom_image(aes(image = plastic), color = "blue", size = 0.03) +
  geom_text(aes(y = percapita_mismanaged_kg_pp_pd, label = entity), hjust = 0, nudge_y = 0.007,
  family = "IBM Plex Sans", size = 2) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 2)) +
  labs(title = "Mismanaged plastic waste in 2010",
       subtitle = "kilograms per person per day ",
       caption = "Source: Our World In Data | Graphic: Georgios Karamanis / @geokaramanis") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    panel.grid.major.x = element_line(color = "gray85", size = 0.3),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(1, 0.2, 1, 0.2), "cm"),
    axis.ticks.x = element_line(color = "#212121", size = 0.3),
    axis.line.x = element_line(size = 0.3, color = "#212121"),
    text = element_text(family = "IBM Plex Sans", size = 8),
    plot.title = element_text(face = "bold", vjust = 8),
    plot.subtitle = element_text(vjust = 9),
    plot.caption = element_text(size = 5, vjust = -3)
  )

ggsave("./week-21/waste.png", height = 10, width = 5)
