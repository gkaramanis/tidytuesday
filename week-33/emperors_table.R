library(tidyverse)
library(here)
library(lubridate)
library(glue)
library(ggtext)
library(rcartocolor)
library(cowplot)

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
    age_death = round(interval(birth, death) / years(1)),
    age_reignstart = interval(birth, reign_start) / years(1),
    age_reignend = interval(birth, reign_end) / years(1),
    reign_duration = round(interval(reign_start, reign_end) / years(1), 1),
    nam = substring(name, 1, 3),
    name = case_when(
    	name == "Consantius II" ~ "Constantius II",
    	TRUE ~ name
    	)
  )

p1 <- ggplot(age_emperors) +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1, fill = dynasty)) +
  geom_rect(aes(xmin = -1, ymin = 0.9, xmax = 1, ymax = 1, fill = era)) +
  # labels
  geom_text(aes(label = name, x = -0.65, y = 0.45), hjust = 0,
            size = 2, family = ("Cinzel"), color = "white") +
  geom_text(aes(label = nam, x = -0.7, y = 0.05), hjust = 0,
            size = 8, family = ("Cinzel"), fontface = "bold", color = "white") +
  geom_text(aes(label = paste0(age_death, " | ", reign_duration),
                x = -0.7, y = -0.5), hjust = 0,
            size = 6, family = ("Cinzel"), color = "grey90") +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  scale_fill_carto_d(palette = "Antique", name = "Eras and Dynasties") +
  facet_wrap(~ index, ncol = 10) +
  labs(
  	title = "The Unperiodic Table\nof the Roman Emperors,\n27 BCE – 395 CE",
  	caption = "Source: Zonination via Wikipedia | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Cinzel") +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(2, "points"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "Cinzel", face = "bold", size = 48,
                              margin = margin(0, 0, 60, 0), hjust = 0),
    plot.caption = element_text(family = "Cinzel", hjust = 0.5, size = 20,
                                margin = margin(40, 0, 0, 50))
  ) 
  
p2 <- ggplot(subset(age_emperors, index == 1)) +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1, fill = dynasty)) +
  geom_rect(aes(xmin = -1, ymin = 0.9, xmax = 1, ymax = 1, fill = era)) +
  # labels
  geom_text(aes(label = name, x = -0.65, y = 0.45), hjust = 0,
            size = 3, family = ("Cinzel"), color = "white") +
  geom_text(aes(label = nam, x = -0.7, y = 0.05), hjust = 0,
            size = 12, family = ("Cinzel"), fontface = "bold", color = "white") +
  geom_text(aes(label = paste0(age_death, " | ", reign_duration),
                x = -0.7, y = -0.5), hjust = 0,
            size = 8, family = ("Cinzel"), color = "grey90") +
  # legend of legend
  geom_label(aes(x = -1.1, y = 0.90, hjust = 1,
                     label = "Era"), size = 4,  family = ("Cinzel"), fontface = "bold", fill = "#E0AF5B", color = "white") +
  geom_label(aes(x = -1.1, y = 0.7, hjust = 1,
                     label = "Dynasty"), size = 4,  family = ("Cinzel"), fontface = "bold", fill = "#8D5776", color = "white") +
  geom_text(aes(x = -1.1, y = 0.45, hjust = 1,
                     label = "Full Name"), size = 4,  family = ("Cinzel"), color = "black") +
  geom_text(aes(x = -1.1, y = 0.05, hjust = 1,
                     label = "Abbreviation"), size = 6,  family = ("Cinzel"), fontface = "bold", color = "black") +
  geom_text(aes(x = -1.1, y = -0.5, hjust = 1,
                     label = "Age at Death | Reign Duration\n(in years)"), size = 4,  family = ("Cinzel"), color = "black") +
  
  coord_fixed(xlim = c(-4, 1), ylim = c(-1, 1)) +
  scale_fill_carto_d(palette = "Antique", name = "Eras and Dynasties") +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

ggdraw(p1) + draw_plot(p2, 0.49, 0.81, 0.6, 0.15) +
  
ggsave(here::here("week-33", "img_plot", paste0("emperors", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         # width = 15, height = 15, dpi = 320)
       width = 15, height = 15)







  