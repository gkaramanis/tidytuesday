library(tidyverse)
library(osmdata)
library(MetBrewer)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv') %>% 
  janitor::clean_names() %>% 
  mutate(
    date = lubridate::mdy(survey_date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    sex = if_else(female == 0, "male\nfrogs", "female\nfrogs")
  )


# Code from https://rpubs.com/scolando/Tidy-Tuesday-08-02-2022
crane_prairie <- opq(bbox = c(-121.7920729, 43.7938767, -121.76501, 43.81433)) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

cr_pr_w <- crane_prairie$osm_polygons %>% 
  sf::st_transform(crs = "EPSG: 32610")

# Make subsite labels
sub_labels <- frogs %>% 
  group_by(subsite) %>% 
  summarise(
    utme_83 = mean(utme_83),
    utmn_83 = mean(utmn_83),
    month = "September",
    sex = "female\nfrogs"
    ) %>% 
  ungroup()
  
f1 <- "Outfit"

ggplot(frogs) +
  geom_sf(data = cr_pr_w, fill = "azure3", color = "azure4", size = 0.1) +
  ggrepel::geom_text_repel(data = sub_labels, aes(utme_83, utmn_83, label = subsite), family = f1, color = "cornsilk4", seed = 9) +
  geom_bin2d(aes(utme_83, utmn_83), bins = 20, alpha = 0.7) +
  scale_fill_stepsn(colors = met.brewer("Tam"), n.breaks = 10, guide = guide_colorsteps(title.position = "top")) +
  coord_sf() +
  facet_grid(vars(sex), vars(factor(month, levels = month.name))) +
  labs(
    title = "Late-season movement and habitat use by Oregon spotted frogs at Crane Prairie Reservoir in Oregon",
    subtitle = "Pearl, C.A., Rowe, J.C., McCreary, B., and Adams, M.J., 2022, Oregon spotted frog (Rana pretiosa) telemetry and habitat use at Crane Prairie Reservoir in Oregon, USA\nU.S. Geological Survey data release, https://doi.org/10.5066/P9DACPCV",
    caption = "Source: USGS · Graphic: Georgios Karamanis",
    fill = "Number of frogs"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "lines"),
    legend.title.align = 0.5,
    plot.background = element_rect(fill = "grey90", color = NA),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(margin = margin(5, 0, 20, 0), size = 8, lineheight = 1),
    plot.caption = element_text(color = "grey40"),
    strip.text.x = element_text(size = 12, face = "bold", margin = margin(5, 0, 5, 0)),
    strip.text.y = element_text(size = 10, color = "grey30", hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )
  
  
