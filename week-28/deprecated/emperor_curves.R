library(tidyverse)
library(here)
library(lubridate)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

# BCE dates
age_emperors <- emperors %>% 
  mutate(
    birth = case_when(
      index %in% c(1, 2, 4, 6) ~ update(birth, year = -year(birth)),
      TRUE ~ birth
    ),
    reign_start = case_when(
      index == 1 ~ update(reign_start, year = -year(reign_start)),
      TRUE ~ reign_start
    ),
    age_death = interval(birth, death) / years(1),
    age_reignstart = interval(birth, reign_start) / years(1),
    age_reignend = interval(birth, reign_end) / years(1) 
  ) %>% 
  filter(!is.na(age_death)) %>% 
  mutate(index = row_number())

ggplot(age_emperors) +
  geom_curve(aes(x = reign_start, y = 1,
                 xend = reign_end, yend = 1, color = name),
             curvature = -1) +
  geom_curve(aes(x = birth, y = -1,
                 xend = death, yend = -1, color = name),
             curvature = +1) +
  ylim(-15, 15) +
theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#9a1d15", color = "#9a1d15")
  ) +
  
  ggsave(here::here("week-33", "img_plot", paste0("emperors", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         width = 18, height = 6, dpi = 320)


