library(tidyverse)
library(ggimage)
library(ggforce)

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

women_angle <- women %>% 
  arrange(category) %>% 
  mutate(name = fct_reorder(name, category)) %>% 
  mutate(
    n = row_number(),
    angle = seq(0, 360 - 360/length(name), length.out = length(name)),
    x = cos(angle * pi / 180),
    y = sin(angle * pi / 180),
    image = here::here("2020-week50", paste0("images/", name, ".png")),
    img_offset = rep(c(0.775, 0.875), length.out = length(name))
  )
  
ggplot(women_angle) +
  geom_image(data = NULL, aes(x = 0, y = 0, image = here::here("2020-week50/images/Unsung hero.png")), size = 0.33) +
	geom_text(aes(x = x, y = y, label = name, angle = ifelse(between(angle, 90, 270), angle - 180, angle), hjust = ifelse(between(angle, 90, 270), 1, 0), color = category), family = "Atkinson Hyperlegible Bold") +
  geom_image(data = subset(women_angle, name != "Unsung hero"), aes(x = img_offset * x, y = img_offset * y, image = image), size = 0.02) +
  geom_segment(data = subset(women_angle, name != "Unsung hero"), aes(x = 0.98 * x, y = 0.98 * y, xend = (0.05 + img_offset) * x, yend = (0.05 + img_offset) * y, color = category), size = 0.5) +
  geom_circle(data = subset(women_angle, name != "Unsung hero"), aes(x0 = img_offset * x, y0 = img_offset * y, r = 0.045, color = category), n = 40, size = 0.5) +
  geom_circle(data = subset(women_angle, name == "Unsung hero"), aes(x0 = 0, y0 = 0, r = 0.67, color = category), n = 70, size = 1.4) +
  geom_segment(data = subset(women_angle, name == "Unsung hero"), aes(x = 0.68 * x, y = 0.68 * y, xend = x - 0.02, yend = y, color = category), size = 1.1) +
  coord_fixed(clip = "off", xlim = c(-1.8, 1.8), ylim = c(-1.8, 1.8)) +
  scale_color_manual(values = c('#e6194B', '#4363d8', '#f58231', '#911eb4', '#800000')) +
  annotate("text", x = 0, y = 0, label = "BBC's 100\n\n\n\nWomen of 2020", family = "DIN Condensed Bold", size = 13, lineheight = 1) +
  labs(
    caption = "Data: BBC | Graphic: Georgios Karamanis"
  ) +
	theme_void() +
	theme(
		legend.position = "none",
		plot.background = element_rect(fill = "white", color = NA),
		plot.caption = element_text(hjust = 0.5, family = "DIN Condensed Bold", margin = margin(-35, 0, 0, 0), color = "grey50", size = 12)
	) +
  ggsave(here::here("temp", paste0("women-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 12)
