library(tidyverse)
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
	unnest(c(y, y1, y2))

cup <- data.frame(
  x = c(-0.35, 0.35, 0.34, 0.25, -0.25, -0.34),
  y = c(0.7, 0.7, 0, -1.2, -1.2, 0)
  )

ggplot(varieties) +
# Aroma -------------------------------------------------------------------
  geom_path(aes(x = 0.15 * cos(3 * y + a), y = y, group = variety), size = 1, lineend = "round", colour = lighten("#6f4e37", 0.4), alpha = 0.9) +
	geom_path(aes(x = -0.15 + 0.2 * cos(2 * y), y = y1, group = variety), linetype = "longdash", alpha = 0.2, colour = "grey80", lineend = "round") +
	geom_path(aes(x = 0.2 + 0.15 * cos(1.8 * y + a), y = y2, group = variety), linetype = "longdash", alpha = 0.2, colour = "grey80", lineend = "round") +
  geom_text(aes(x = 0, y = mean + 1.25, label = (round(mean, 1))), colour = "#ffeacd", family = "DIN Condensed Bold", size = 6) +
# Aftertaste --------------------------------------------------------------
	geom_segment(aes(x = 0, y = 0, xend = 0, yend = -after_mean), size = 2, lineend = "round", colour = darken("#6f4e37", 0.2)) +
  geom_text(aes(x = 0, y = -after_mean - 0.75, label = round(after_mean, 1)), colour = darken("#6f4e37", 0.5), family = "DIN Condensed Bold", size = 5) +
# Cups --------------------------------------------------------------------
  geom_polygon(data = cup, aes(x, y), size = 7, colour = "#6f4e37", fill = "#6f4e37") + # "shadow"
	geom_polygon(data = cup, aes(x, y), size = 4, colour = "grey95", fill = "grey95") +
	geom_text(aes(x = 0, y = 0.55, label = variety), colour = "black", family = "DIN Condensed Bold", vjust = 1, lineheight = 1, size = 4.5, check_overlap = TRUE) +
# Theme, etc --------------------------------------------------------------
	coord_cartesian(clip = "off", xlim = c(-0.6, 0.6), ylim = c(-9, 9)) +
	facet_wrap(vars(variety), nrow = 1) +
  labs(
    title = "Aroma and aftertaste",
    subtitle = str_wrap("Mean values of gradings by the Coffee Quality Institute, for varieties with 10 or more gradings. For aroma, the standard deviation is shown with dashed lines.", 80),
    caption = "Source: Coffee Quality Database | Graphic: Georgios Karamanis"
    ) +
	theme_void(base_family = "DIN Condensed Bold") +
	theme(
		  plot.background = element_rect(fill = "#6f4e37", colour = NA),
			panel.spacing = unit(1.4, "lines"),
			strip.text = element_blank(),
			plot.title = element_text(hjust = 0.5, size = 28),
			plot.subtitle = element_text(hjust = 0.5, size = 16, family = "IBM Plex Sans", margin = margin(10, 0, 30, 0)),
			plot.caption = element_text(family = "IBM Plex Sans Light", colour = "grey90", size = 10),
			plot.margin = margin(20, 20, 20, 20)
		) +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 15, height = 8)




	