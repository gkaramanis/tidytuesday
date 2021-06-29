library(tidyverse)
library(ggforce)
library(colorspace)
library(futurevisions)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# Palette
pal <- futurevisions("cancri")

# Female astronauts
hrs_mission <- astronauts %>% 
  filter(sex == "female") %>% 
  distinct(name, total_hrs_sum) %>% 
  mutate(
    a = seq(pi/12, pi - pi/12, length.out = nrow(hrs_mission)),
		offset = 2000,                # make space for the Earth
		x = (total_hrs_sum + offset) * cos(a),  # convert to polar
		y = (total_hrs_sum + offset) * sin(a),
		colour = if_else(total_hrs_sum > 6000, pal[4], pal[2]), # highlight some
		colour = if_else(name == "Tereshkova, Valentina", pal[4], colour)
	)

# Gridlines around Earth
gridlines <- data.frame(
  r = c(seq(0, 8000, by = 2000), 15000),
  offset = 2000
  ) %>% 
  mutate(label = case_when(
    r == 0 ~ "",
    r == 15000 ~ paste0(r, " hours"),
    TRUE ~ as.character(r)
  ),
  x = c(rep(0, 5), 500)
  )

stars = data.frame(
  x = runif(150, min = -11000, max = 11000),
	y = runif(150, min = 9000, max = 28000)
)


ggplot(hrs_mission) +
  # Earth
  geom_arc(aes(x0 = 0, y0 = 0, r = offset - 100, start = -pi/2, end = pi/2)) +
  # Stars
	geom_point(data = stars, aes(x = x, y = y, alpha = y), size = 0.75, colour = "grey95", stroke = 0) +
  # Gridlines
  geom_arc(data = gridlines, aes(x0 = 0, y0 = 0, r = r + offset, start = -pi/2, end = pi/2), colour = darken(pal[6]), size = 0.1) +
  geom_text(data = gridlines, aes(x = x, y = r + offset + 400, label = label), size = 2.2, family = "Proxima Nova Light", colour = lighten(pal[1], 0.3)) +
  # Border
  annotate("rect", xmin = -Inf, xmax = +Inf, ymin = -Inf, ymax = Inf, fill = NA, colour = pal[6]) +
  # Astronauts
	geom_point(aes(x, y, size = colour, colour = colour)) +
  # Annotation
  # Peggy Whitson
  annotate("text", x = -3300, y = 14600, label = "Peggy Whitson", colour = pal[6], family = "Proxima Nova", size = 3, hjust = 0) +  
  annotate("text", x = -3300, y = 14000, label = "15982 hours, or 625 days, during 3 missions", colour = pal[6], family = "Proxima Nova Light", size = 2.5, hjust = 0, vjust = 1, lineheight = 1) +
  annotate("segment", x = -3550, y = 14650, xend = -4650, yend = 17000, size = 0.25, colour = darken(pal[1], 0.5)) +
  # Sunita Williams
  annotate("text", x = -5044, y = 8010, label = "Sunita Williams\n7722 hours", family = "Proxima Nova Light", size = 2, hjust = 0, vjust = 0.8, lineheight = 0.9, colour = pal[6]) +
  # Christina Koch
  annotate("text", x = -8480, y = 2800, label = "Christina Koch\n7372.3 hours", family = "Proxima Nova Light", size = 2, hjust = 0, vjust = 0.8, lineheight = 0.9, colour = pal[6]) +
  # Kathleen Rubins
  annotate("text", x = -7635, y = 3695, label = "Kathleen Rubins\n6902.35 hours", family = "Proxima Nova Light", size = 2, hjust = 0, vjust = 0.8, lineheight = 0.9, colour = pal[6]) +
  # Valentina Tereshkova
  annotate("text", x = 3000, y = 2900, label = "Valentina Tereshkova\nThe first and youngest woman in space\n70.83 hours, during one mission in 1963", family = "Proxima Nova Light", size = 2, hjust = 0, vjust = 0.8, lineheight = 0.9, colour = pal[6]) +
  annotate("segment", x = 2800, y = 2350, xend = 2100, yend = 800, size = 0.25, colour = darken(pal[1], 0.5)) +
  # Title, subtitle
  annotate("text", x = 0, y = 26300, label = toupper("Women in Space"), size = 10, family = "Proxima Nova Extrabold", colour = pal[5]) +
  annotate("text", x = 0, y = 24200, label = toupper("Total duration of all missions\n1961-2019"), size = 4, family = "Proxima Nova Bold", colour = pal[5], lineheight = 1.1) +
  # Caption
  annotate("text", x = 10800, y = 7000, angle = 90, hjust = 0, vjust = 0, label = "Source: M. Stavnichuk & T. Corlett (https://doi.org/10.1016/j.lssr.2020.06.003) | Graphic: Georgios Karamanis",  family = "Proxima Nova Light", size = 1.8, colour = pal[6]) +
  # Scales, theme, etc
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_manual(values = c(0.75, 1.75)) +
	scale_alpha(range = c(0.01, 0.6)) + # for stars
  scale_x_continuous(limits = c(-11000, 11000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 28000), expand = c(0, 0)) +
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[1], colour = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) 

ggsave(here::here("2020-week29", "plots", "temp", paste0("astronauts-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 4.55, height = 5.7)
  
