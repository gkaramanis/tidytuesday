library(tidyverse)
library(ggimage)
library(janitor)

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
mismanaged_vs_gdp <- clean_names(mismanaged_vs_gdp)

mismanaged_vs_gdp %>%
  rename(percapita_mismanaged_kg_pp_pd = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day) %>% 
  filter(entity != "World" & year == 2010) %>% 
  top_n(30, percapita_mismanaged_kg_pp_pd) %>% 
  mutate(plastic = sample(c("https://image.flaticon.com/icons/png/128/81/81940.png",
                            "https://image.flaticon.com/icons/png/128/1758/1758890.png",
                            "https://image.flaticon.com/icons/png/128/960/960773.png"),
                          size =  nrow(.), replace = TRUE)) %>% 
  ggplot(aes(x = gdp_per_capita_ppp_constant_2011_international_rate,
             y = total_population_gapminder,
             size = percapita_mismanaged_kg_pp_pd)) +
  geom_point() +
  scale_y_log10()
  
