library(tidyverse)
library(lubridate)
library(gghighlight)
library(ggmap)


locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

movement <- locations %>% 
  filter(study_site != "Hart Ranges") %>% 
  mutate(
    season = fct_rev(season), # reverse seasons
    longitude = round(longitude, 2), # round long, lat to reduce number of points
    latitude = round(latitude, 2)
  ) %>% 
  distinct(season, study_site, longitude, latitude)

ggmap(get_stamenmap(bbox = c(min(movement$longitude) - .5,
                             min(movement$latitude) - .5,
                             max(movement$longitude) + .5, 
                             max(movement$latitude) + .5),
                    zoom = 8, # scale = 2,
                    maptype ='terrain-lines',
                    color = 'color')) +
  geom_point(data = movement, aes(longitude, latitude, group = study_site, colour = study_site), size = 0.1, alpha = 0.5) +
  gghighlight(unhighlighted_params = list(colour = "grey70"), use_direct_label = FALSE) +
  scale_colour_manual(values = c('#ffe119', '#4363d8', '#f58231', '#e6194B', '#800000', '#000075', '#f032e6', '#3cb44b'), breaks = c("Graham", "Scott", "Moberly", "Burnt Pine", "Kennedy", "Quintette", "Narraway")) +
  guides(colour = guide_legend(title = "Herd", override.aes = list(size = 3))) +
  coord_fixed(ratio = 1.5) +
  facet_wrap(vars(season), ncol = 2) +
  labs(
    title = "Migration patterns of Northern Caribou\nin the South Peace of British Columbia",
    subtitle = str_wrap("In summer, most caribou migrate towards the central core of the Rocky Mountains where they use alpine and subalpine habitat. The result of this movement to the central core of the Rocky Mountains is that some of the east side herds can overlap with west side herds during the summer.", 100),
    caption = str_wrap("Source: Seip DR, Price E (2019) Data from: Science update for the South Peace Northern Caribou (Rangifer tarandus caribou pop. 15) in British Columbia. Movebank Data Repository. https://doi.org/10.5441/001/1.p5bn656k | Graphic: Georgios Karamanis", 70)
  ) +
  theme_void() +
  theme(
    legend.position = c(0.5, 0.6),
    legend.text = element_text(size = 11, colour = "#F9EED9", family = "IBM Plex Sans Condensed Light"),
    legend.title = element_text(size = 16, hjust = 0.5, colour = "#F9EED9", family = "IBM Plex Sans Condensed Medium"),
    panel.spacing.x = unit(3, "lines"),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "#7A6A4F", colour = NA),
    strip.text = element_text(family = "IBM Plex Sans Condensed Medium", colour = "#F9EED9", size = 18),
    plot.title = element_text(family = "IBM Plex Serif Bold", colour = "white", size = 20, hjust = 0, lineheight = 1),
    plot.subtitle = element_text(family = "IBM Plex Sans", colour = "white", size = 12, hjust = 0, lineheight = 1, margin = margin(10, 0, 50, 0)),
    plot.caption = element_text(family = "IBM Plex Sans", colour = "grey80", size = 7, hjust = 1, margin = margin(30, 0, 10, 0))
  ) +
  ggsave(here::here("2020-week26", "plots", "temp", paste0("caribou-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 11)
