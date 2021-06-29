library(tidyverse)
library(here)
library(ggimage)
library(ggrepel)

airplane_png <- here("week-30", "747.png")
components <-  read.csv(here("week-30", "components.csv"))

air_size = 10
  
comp_coord <-  tribble(
  ~aircraft_component, ~x, ~y,
  "Radome", 0, 0.9*air_size,
  "Nose", 0, 0.82*air_size,
  "Windshield", 0, 0.77*air_size,
  "Fuselage", 0, 0.4*air_size,
  "Propeller", 0.36*air_size, 0.25*air_size,
  "Engines", 0.6*air_size, 0.05*air_size,
  "Wing/rotor", 0.7*air_size, -0.2*air_size,
  "Landing gear", 0, -0.05*air_size,
  "Tail", 0, -0.7*air_size
  )

components <- right_join(components, comp_coord)
y_list <- seq(from = 0.82*air_size, to = -0.7*air_size, length.out = 9) 
 
ggplot(components, aes(x, y)) +
  # airplane
  geom_image(aes(image = airplane_png, -4, 0), size = 0.55, asp = 1.3) +
  
  # rectangle
  geom_tile(aes(x = 12.8, y = 0.6,
                width = 9.6, height = 16.8), 
            fill = "#4089bb") +
  
  # labels
  geom_segment(aes(x = x-4, y = y,
    xend = 8, yend = y_list),
    color = "#43464B") +
  geom_text(aes(label = toupper(aircraft_component), 
                x = 8.5, y = y_list), family = "IBM Plex Sans Condensed Medium",
            color = "white", hjust = 0) +
  
  # birds circles
  geom_point(aes(14, y_list, size = birds_percentage_of_total_struck, 
                 stroke = birds_percentage_of_total_damaged/3),
             shape = 21, color = "#E6C4A1", fill = "#69108a") +
  
  # terrestrial mammals circles
  geom_point(aes(16.2, y_list, size = terrestrialmammals_percentage_of_total_struck,
                 stroke = terrestrialmammals_percentage_of_total_damaged/3), 
             shape = 21, color = "#FFFC31", fill = "#3A533D") +
  
  # legend birds
  geom_point(aes(21.5, 4.5, size = 35, 
                 stroke = 6),
             shape = 21, color = "#E6C4A1", fill = "#69108a") +
  geom_segment(aes(x = 21.5, y = 4.5, xend = 21.5, yend = 6),
               color = "#69108a", size = 1) +
  geom_segment(aes(x = 21.5, y = 4, xend = 21.5, yend = 3),
               color = "#E6C4A1", size = 1) +
  geom_text(aes(label = "Birds", 21, 7.6),
            family = "IBM Plex Sans Condensed Medium", 
            hjust = 0, color = "white") +
  geom_text(aes(label = "percent of\ntotal struck", 21, 6.8),
            family = "IBM Plex Sans Condensed",
            lineheight = 0.8, hjust = 0, color = "#43464B") +
  geom_text(aes(label = "percent of\ntotal damaged", 21, 2.2),
            family = "IBM Plex Sans Condensed",
            lineheight = 0.8, hjust = 0, color ="#43464B") +
  # legend terrestrial mammals
  geom_point(aes(21.5, -2.7, size = 35, 
                 stroke = 6),
             shape = 21, color = "#FFFC31", fill = "#3A533D") +
  geom_segment(aes(x = 21.5, y = -2.7, xend = 21.5, yend = -1.2),
               color = "#3A533D", size = 1) +
  geom_segment(aes(x = 21.5, y = -3.2, xend = 21.5, yend = -4.2),
               color = "#FFFC31", size = 1) +
  geom_text(aes(label = "percent of\ntotal struck", 21, -0.4),
            family = "IBM Plex Sans Condensed",
            lineheight = 0.8, hjust = 0, color = "#43464B") +
  geom_text(aes(label = "percent of\ntotal damaged", 21, -5),
            lineheight = 0.8, family = "IBM Plex Sans Condensed",
            hjust = 0, color = "#43464B") +
  geom_text(aes(label = "Terrestrial\nMammals", 21, -6.2),
            family = "IBM Plex Sans Condensed Medium",
            lineheight = 0.8, hjust = 0, color = "white") +
  
  labs(
    title = "Aircraft components most commonly reported as struck and damaged by birds and terrestrial mammals",
    subtitle = "The aircraft components most commonly reported as struck by birds from 1990 – 2017 were the nose/radome, windshield,\nwing/rotor, engine, and fuselage. Aircraft engines were the component most frequently reported as being damaged by bird\nstrikes (27 percent of all damaged components). Aircraft components most commonly reported as struck by terrestrial\nmammals were the landing gear, 'other', propeller, and wing/rotor. Aircraft components most commonly reported as\ndamaged were the landing gear, wing/rotor, propeller, and 'other' (Wildlife Strikes to Civil Aircraft in the United States,\n1990–2017).",
    caption = "Source: FAA National Wildlife Strike Database | Graphic: Georgios Karamanis"
  ) +

  scale_size_area(max_size = 8) +
  coord_fixed(xlim = c(-5, 25), ylim =  c(-10, 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20),
    plot.title  = element_text(family = "IBM Plex Sans Bold", color = "white"),
    plot.subtitle = element_text(family = "IBM Plex Serif", color = "white"),
    plot.caption  = element_text(family = "IBM Plex Sans"),
    plot.background = element_rect(color = "skyblue3", fill = "skyblue3")
  ) 

ggsave(here("week-30", "img", paste0("wildlife", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         width = 10, height = 8.32)
