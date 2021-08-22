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
    birth = year(birth),
    death = year(death),
    reign_start = year(reign_start),
    reign_end = year(reign_end),
    reign_duration = reign_end - reign_start
  )

ggplot(age_emperors) +
  geom_tile(aes(x = 0, y = index,
                width = reign_duration, height = 0.8))
  
  ggsave(here::here("week-33", "img_plot", paste0("emperors", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         width = 12, height = 12, dpi = 320)



#9a1d15 60.9 %Sangria (Red)
#b89836 17.8 % Sundance (Brown)
#d3b03c 8.2 % Metallic Gold (Yellow)
#8d2516 5.1 % Falu Red (Red)
#925523 4.5 % Mai Tai (Brown)
#a67831 3.4 % Hot Toddy (Brown)



