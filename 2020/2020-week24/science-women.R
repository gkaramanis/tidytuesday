library(tidyverse)
library(ggforce)
library(shadowtext)

science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

science_names <- science %>% 
  separate(name, sep = ", ", into = c("last_name", "first_name"), extra = "merge") %>% 
  mutate(
    n = row_number(),
    last_name = if_else(str_detect(first_name, "Jr."), paste0(last_name, " Jr."), last_name),
    first_name = str_remove(first_name, ", Jr."),
    full_name = paste(first_name, last_name),
    occupation_s = str_remove(occupation_s, "\\[citation needed\\]"),
    first_name1 = str_extract(first_name, "\\w*")
  )

women = tibble(n = c(6, 9, 10, 19, 26, 33, 38, 42, 48, 52, 54, 61, 74, 75, 90, 104, 106, 107, 108, 110, 118, 120))

science_w <- science_names %>% 
  semi_join(women) %>% 
  mutate(
    n = row_number(),
    colour = if_else(is.na(birth), "grey60", "white"),
    # name_colour = if_else(is.na(birth), "grey50", "black"),
    birthdeath = case_when(
      is.na(birth) ~ "Unknown",
      is.na(death) ~ paste0(birth, "-"),
      TRUE ~ paste0(birth, "-", death)
    ),
    birth_arc = replace_na(birth, 1832),
    death_arc = replace_na(death, 2020),
    a = (2020 - death_arc) * pi / 190,
    b = (2020 - birth_arc) * pi / 190,
		i = (birth_arc - 2020 - 90 - 8) * pi / 190,
  )

ggplot(science_w) +
  # Female symbol
  geom_arc(aes(x0 = 0, y0 = 0, r = 17, start = 0, end = 2 * pi), size = 65, colour = "black", n = 1800) +
  geom_segment(aes(x = -12, y = -33, xend = 12, yend = -33), size = 50, colour = "black") +
	geom_segment(aes(x = 0, y = -20, xend = 0, yend = -45), size = 50, colour = "black") +
  # Year labels
  geom_segment(aes(x = 0, y = 9, xend = 0, yend = 10), size = 0.5, colour = "grey60", lineend = "round") + 
	geom_text(aes(x = 0, y = 8, label = "2020"), colour = "grey50", family = "DIN Condensed Bold") + 
	geom_segment(aes(x = 0, y = -9, xend = 0, yend = -10), size = 0.5, colour = "grey60", lineend = "round") + 
	geom_text(aes(x = 0, y = -8, label = "1830"), colour = "grey50", family = "DIN Condensed Bold") + 
  # Left arcs, "12 -" is to reverse order
  geom_arc(data = subset(science_w, n < 12), aes(x0 = 0, y0 = 0, r = 12 - n + 11, start = -a, end = -b, colour = colour), size = 3) +
  # Right arcs
  geom_arc(data = subset(science_w, n > 11), aes(x0 = 0, y0 = 0, r = n, start = a, end = b, colour = colour), size = 3) +
	# n labels
	geom_text(data = subset(science_w, n < 12), aes(x = (12 - n + 11) * cos(i), y = -(12 - n + 11) * sin(i), label = n, colour = colour), family = "DIN Condensed Bold") +
	geom_text(data = subset(science_w, n > 11), aes(x = -n * cos(i), y = -n	* sin(i), label = n, colour = colour), family = "DIN Condensed Bold") +
  # Names left and right
  geom_text(data = subset(science_w, n < 12), aes(x = -30, y = seq(20, -40, length.out = 11), label = paste0(n, ". ", full_name)), colour = "black",  family = "DIN Condensed Bold", hjust = 1, size = 8) +
	geom_text(data = subset(science_w, n < 12), aes(x = -30, y = seq(17.5, -42.5, length.out = 11), label = birthdeath), colour = "grey50", family = "DIN Condensed Bold", hjust = 1, size = 8) +
  geom_text(data = subset(science_w, n > 11), aes(x = 30, y = seq(20, -40, length.out = 11), label = paste0(n, ". ", full_name)), colour = "black",  family = "DIN Condensed Bold", hjust = 0, size = 8) +
	geom_text(data = subset(science_w, n > 11), aes(x = 30, y = seq(17.5, -42.5, length.out = 11), label = birthdeath), colour = "grey50", family = "DIN Condensed Bold", hjust = 0, size = 8) +
  # Title
  geom_text(aes(x = 0, y = -31.8, label = toupper("female african american\nscientists and inventors")), size = 9.4, vjust = 0.5, lineheight = 1, family = "DIN Condensed Bold", colour = "white") +
	# Caption
	geom_text(aes(x = 0, y = -36, label = toupper("Source: Wikipedia | Graphic: Georgios Karamanis")), size = 4.7, vjust = 0.5, lineheight = 1, family = "DIN Condensed", colour = "white") +
  # Theme and other stuff
  coord_fixed(clip = "off", xlim = c(-40, 40)) +
  scale_colour_identity() +
	theme_void() +
	theme(
	  legend.position = "none",
	  plot.background = element_rect(fill = "white", colour = "white"),
	  plot.margin = margin(20, 20, 20, 20)
	) 

ggsave(here::here("2020-week24", "plots", "science-women.png"), dpi = 320, height = 11.5, width = 16)

