library(tidyverse)
library(ggridges)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

spam <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')

spam_long <- spam %>% 
  select(-crl.tot) %>% 
  pivot_longer(1:make) %>% 
  mutate(
    value_log = log10(value),
    yesno = if_else(yesno == "y", "Spam", "Not\nSpam")
    ) %>% 
  filter(value_log > -Inf)
  
f1 <- "DIN Condensed"
f2 <- "Outfit"

pal <- RColorBrewer::brewer.pal(3, "Pastel2") %>% 
  colorspace::darken(., 0.5)

ggplot(spam_long) +
  geom_density_ridges(aes(value, y = yesno, fill = yesno, color = after_scale(colorspace::darken(fill, 0.5))), quantile_lines = TRUE, quantiles = 2, jittered_points = TRUE, scale = 1, rel_min_height = .01, point_shape = "|", point_size = 3, size = 0.25, position = position_points_jitter(height = 0, yoffset = -0.2)) +
  scale_fill_brewer(palette = "Pastel2") +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(name), ncol = 1, scales = "free_x") +
  labs(
    title = "Spam E-mail",
    subtitle = "Occurrences of '!', '$', 'make', 'money', and '000' as a percent of total number of words\nin 4601 emails, of which 1813 were identified as spam",
    caption = "Source: UCI Repository Of Machine Learning Databases · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(color = pal),
    axis.text.x = element_text(color = "grey50"),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 12, family = f2, face = "bold"),
    plot.margin = margin(10, 30, 10, 30),
    plot.title = element_text(family = f2, face = "bold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = f2, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(family = f2, margin = margin(20, 0, 0, 0))
  )


