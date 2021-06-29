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
  geom_rect(aes(ymin = 0, xmin = index - 0.1, ymax = age_death, xmax = index + 0.1), fill = "#a67831") +
  geom_rect(aes(ymin = age_reignstart, xmin = index - 0.25, ymax = age_reignend, xmax = index + 0.25), fill = "#d3b03c") +
  # cause of death as icon/label
	# background bands = dynasties
  scale_y_reverse(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
		theme_void() +
		theme(
			plot.background = element_rect(fill = "#9a1d15", color = "#9a1d15"),
			axis.text.y = element_text(color = "white")
		) 

ggsave(here::here("week-33", "img_plot", paste0("emperors", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
        width = 12, height = 12, dpi = 320)
        
        
        
#9a1d15 60.9 %Sangria (Red)
#b89836 17.8 % Sundance (Brown)
#d3b03c 8.2 % Metallic Gold (Yellow)
#8d2516 5.1 % Falu Red (Red)
#925523 4.5 % Mai Tai (Brown)
#a67831 3.4 % Hot Toddy (Brown)



