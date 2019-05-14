library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  filter(!is.na(death_country)) %>%
  filter(birth_country != death_country) %>% 
  ggplot(aes(y = fct_rev(factor(birth_country)), x = 0,
         xend = death_country, yend = 1)) +
  # geom_curve(data=subset(nobel_winners, birth_country > death_country), 
             # curvature = 1, alpha = 0.3) +
  geom_curve(
    curvature = -0.5,
    alpha = 0.1,
    arrow = arrow(length = unit(0.01, "npc"))
    ) +
  scale_x_discrete() +
  scale_y_discrete() +
  # facet_grid(category ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank()
  )


nobel_winners %>% 
  filter(!is.na(death_country)) %>% 
  mutate(diffCountry = ifelse(birth_country == death_country, 0, 1)) %>% 
  group_by(death_country) %>% 
  tally(diffCountry) %>% 
  arrange(desc(n))
         