library(tidyverse)
library(ggforce)
library(colorspace)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

varieties <- coffee_ratings %>%
	group_by(variety) %>%
	summarize(n = n(), mean = mean(aroma), sd = sd(aroma), after_mean = mean(aftertaste)) %>%
	filter(n > 9) %>%
	filter(!is.na(variety) & variety != "Other") %>%
	rowwise() %>%
	mutate(
		a = runif(1) * 5,
		y = list(seq(0, mean, length.out = 100)),
		y1 = list(seq(0, mean - sd, length.out = 100)),
		y2 = list(seq(0, mean + sd, length.out = 100))
		) %>%
	ungroup() %>%
  mutate(
    variety = str_replace(variety, " ", "\n"),
    variety = fct_reorder(variety, mean)
  ) %>% 
  group_by(variety) %>% 
  mutate(variety_n = cur_group_id()) %>% 
  ungroup() %>% 
	unnest(c(y, y1, y2))

cups <- tibble(n = 0:11) %>% 
  rowwise() %>% 
  mutate(
  x = list(n + c(0.75, 1.25, 1.25, 1.23, 1.2, 1.14, 1.1, 0.9, 0.86, 0.8, 0.76, 0.75)),
  y = list(c(-0.15, -0.15, -0.2, -0.45, -0.75, -1.15, -1.15, -1.15, -1.15, -0.75, -0.45, -0.2))
) %>% 
  unnest(c(x, y))


ggplot(varieties) +
# Aroma -------------------------------------------------------------------
  geom_path(aes(x = variety_n + 0.1 * cos(3 * y + a), y = y, group = variety), size = 1, lineend = "round", colour = "#c7b198", alpha = 1) +
# Aroma ± sd --------------------------------------------------------------
	geom_path(aes( x =  variety_n + -0.12 + 0.1 * cos(2 * y), y = y1, group = variety), linetype = "longdash", alpha = 0.3, colour = "#dfd3c3", lineend = "round") +
	geom_path(aes(x =  variety_n + 0.12 + 0.12 * cos(1.8 * y + a), y = y2, group = variety), linetype = "longdash", alpha = 0.4, colour = "#dfd3c3", lineend = "round") +
# Aroma label -------------------------------------------------------------
  geom_text(aes(x = variety_n - 0.2, y = mean, label = (round(mean, 1))), colour = "#f0ece3", family = "DIN Condensed Bold", size = 6) +
  geom_pointrange(aes(x = variety_n - 0.37, y = mean, ymin = mean + sd, ymax = mean - sd), size = 0.4, fatten = 2, colour = darken("#f0ece3")) +
# Cups ---------------------------------------------------------------------
  geom_bspline_closed(data = cups, aes(x, y, group = n), fill = NA, colour = "grey10") +
# Variety label -----------------------------------------------------------
	geom_text(aes(x = variety_n, y = -1.5, label = variety), colour = "#f0ece3", family = "DIN Alternate Bold", vjust = 1, lineheight = 1, size = 5, check_overlap = TRUE) +
# Theme, etc --------------------------------------------------------------
	coord_cartesian(clip = "off", xlim = c(0.75, 12.25), ylim = c(-2, 8.5)) +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = (1:8), labels = (1:8), position = "right") +
  labs(
    title = "Coffee Aroma",
    subtitle = str_wrap("Mean value (with standard deviation) of gradings by the Coffee Quality Institute, for varieties with 10 or more gradings", 65),
    caption = "Source: Coffee Quality Database | Graphic: Georgios Karamanis"
    ) +
	theme_void(base_family = "DIN Condensed Bold") +
	theme(
		  plot.background = element_rect(fill = "#596e79", colour = NA),
			strip.text = element_blank(),
			panel.grid.major.y = element_line(colour = darken("#596e79", 0.2)),
			axis.text.y = element_text(margin = margin(0, 0, 0, 10), colour = darken("#596e79", 0.3), size = 14),
			plot.title = element_text(hjust = 0.5, size = 28, colour = "white"),
			plot.subtitle = element_text(hjust = 0.5, size = 16, family = "IBM Plex Sans", margin = margin(10, 0, 0, 0), colour = "#f0ece3"),
			plot.caption = element_text(family = "IBM Plex Sans Light", colour = "#f0ece3", size = 10, margin = margin(20, 0, 0, 0)),
			plot.margin = margin(20, 20, 20, 20)
		) +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 7)




	