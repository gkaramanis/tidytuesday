library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 12, units = "in", dpi = 320)

wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')

goal_diff <- wcmatches %>% 
  mutate(
    goal_diff = home_score - away_score,
    col = case_when(
      goal_diff > 0 ~ "#519AA7", 
      goal_diff < 0 ~ "#ED853D",
      TRUE ~ "grey20"
    )
    )

f1 <- "Outfit"

ggplot(goal_diff) +
  geom_histogram(aes(x = goal_diff, fill = col), binwidth = 1) +
  scale_x_continuous(breaks = seq(-10, 10, 5)) +
  scale_fill_identity() +
  facet_wrap(vars(year), ncol = 3, strip.position = "bottom") +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  )
  