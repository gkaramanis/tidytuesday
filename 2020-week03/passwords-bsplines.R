library(tidyverse)
library(here)
library(ggforce)
library(wesanderson)
library(tidylog)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>% 
  na.omit()

# Make "circles" (4 pairs of x and y values for the bspline), add some random variation 
bsplines <- passwords %>% 
  mutate(
    r = strength/100,
    x = lapply(r/2, function(x)
    {y = rnorm(4, 1, r/5)
    x*c(-1, 1, 1, -1) + y}),
    y = lapply(r/2, function(x)
    {y = rnorm(4, 1, r/5)
    x*c(1, 1, -1, -1) + y})
    ) %>%
  unnest()

ggplot(bsplines) +
  geom_bspline_closed(aes(x, y, group = password, color = strength, size = strength), fill = NA) +
  scale_color_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"), guide = guide_colorbar(title = "STRENGTH", title.position = "top", title.hjust = 0.5, barheight = 0.7)) +
  scale_size(range = c(0, 0.7), guide = FALSE) +
  facet_wrap(vars(toupper(category)), ncol = 5) +
  coord_fixed() +
  labs(
    title = toupper("Strength of passwords by category"),
    subtitle = "Every 'circle' is a password. Radius, stroke width, and color represent its strength.",
    caption = "Source: Information is Beautiful | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Mono") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, color = "grey40", margin = margin(0, 0, 10, 0)),
    plot.title = element_text(size = 18, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(size = 8,  color = "grey60", margin = margin(0, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("2020-week03", "plots", "temp", paste0("passwords-bsplines-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 13, height = 8
)
