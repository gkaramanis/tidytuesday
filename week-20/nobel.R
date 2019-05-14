library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  filter(!is.na(death_country)) %>%
  # filter(birth_country != death_country) %>% 
  ggplot(aes(fct_rev(factor(birth_country)), x = 0,
         xend = death_country, yend = 0)) +
  # geom_curve(data=subset(nobel_winners, birth_country > death_country), 
             # curvature = 1, alpha = 0.3) +
  geom_curve(curvature = -0.4, alpha = 0.1) +
  scale_x_discrete() +
  scale_y_discrete() +
  # facet_grid(category ~ .) +
  theme_minimal() +
  theme(
    # axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


nobel_winners %>% 
  filter(!is.na(death_country)) %>% 
  mutate(diffCountry = ifelse(birth_country == death_country, 0, 1)) %>% 
  group_by(category) %>% 
  tally(diffCountry) %>% 
  arrange(desc(n))
         