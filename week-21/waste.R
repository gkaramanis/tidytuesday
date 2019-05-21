library(tidyverse)
library(ggimage)
library(janitor)

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_vs_gdp <- clean_names(mismanaged_vs_gdp)

mismanaged_vs_gdp %>% 
  filter(entity != "World" & year == 2010) %>% 
  top_n(30, per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day) %>% 
  mutate(plastic = sample(c("https://image.flaticon.com/icons/png/128/81/81940.png",
                           "https://image.flaticon.com/icons/png/128/1758/1758890.png",
                           "https://image.flaticon.com/icons/png/128/960/960773.png"),
                          size =  nrow(.), replace = TRUE)) %>% 
  ggplot(aes(fct_rev(factor(entity)), per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day)) +
  geom_image(aes(image = plastic), color = "blue", size=0.05) +
  geom_text(aes(label=entity), hjust = -0.4) +
  coord_flip() +
  labs(title = "Mismanaged plastic waste in 2010",
       subtitle = "kilograms per person per day ",
       caption = "\nSource: Winger et al, 2019 | Graphic: Georgios Karamanis / @geokaramanis") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(1, 0.2, 1, 0.2), "cm"),
    axis.ticks.x = element_line(color = "#212121", size = 0.3),
    axis.line.x = element_line(size = 0.3, color = "#212121")
  )

ggsave("./week-21/waste.png", height = 6, width = 5)
