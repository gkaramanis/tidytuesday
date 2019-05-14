library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

nobel_winners %>%
  # remove NA death countries
  filter(!is.na(death_country)) %>%
  # keep only entries with different birth and death country
  filter(birth_country != death_country) %>% 
  mutate(
    colour = case_when(
      death_country == "United States of America" ~ "#FF2B4F",
      death_country == "West Germany (Germany)" ~ "#003399",
      death_country == "United Kingdom" ~ "#3686d3",
      death_country == "Germany" ~ "#fcab27",
      death_country == "France" ~ "#88398a",
      death_country == "Switzerland " ~ "#88f98a",
      T ~ "gray60"
    )) %>% 
  ggplot(aes(
    x = 0,
    y = fct_rev(factor(birth_country)),
    xend = death_country,
    yend = 1,
    colour = colour,
    alpha = (colour != "gray60"))) +
  geom_curve(
    curvature = -0.5,
    arrow = arrow(length = unit(0.01, "npc"))
    ) +
  scale_x_discrete() +
  scale_y_discrete() +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.1, 0.2), guide = F) +
  scale_size_manual(values = c(0.1, 0.7), guide = F) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1),
    text = element_text(family = "IBM Plex Sans", size = 6),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(vjust = 2)
  ) +
  ggsave("./week-20/nobel.png", width = 4, height = 4)


nobel_winners %>% 
  filter(!is.na(death_country)) %>% 
  mutate(diffCountry = ifelse(birth_country == death_country, 0, 1)) %>% 
  group_by(death_country) %>% 
  tally(diffCountry) %>% 
  arrange(desc(n))
         