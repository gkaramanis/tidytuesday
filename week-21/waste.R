library(tidyverse)
library(ggimage)
# library(rsvg)
library(janitor)

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

coast_vs_waste <- clean_names(coast_vs_waste)
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
  annotate("text", 
           x = mismanaged_vs_gdp$per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
           y = mismanaged_vs_gdp$entity,
           label = mismanaged_vs_gdp$entity
           ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
  )

ggsave("./week-21/waste.png", height = 5, width = 6)
