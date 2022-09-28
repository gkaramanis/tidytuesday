library(tidyverse)
library(camcorder)
library(sf)
library(albersusa)
library(ggpattern)
library(ggrepel)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

top <- artists %>% 
  group_by(state) %>% 
  slice_max(order_by = location_quotient, n = 1) %>% 
  ungroup()

us_sf <- usa_sf("laea") %>% 
  left_join(top, by = c("name" = "state"))

f1 <- "Outfit"
pal <- MetBrewer::met.brewer("Tam")[1:7]

ggplot(us_sf) +
  geom_sf_pattern(aes(pattern = race,
                      pattern_angle = race,
                      pattern_spacing = if_else(race == "Other", 0.04, 0.012),
                      fill = location_quotient,
                      pattern_fill = after_scale(colorspace::lighten(fill, 0.2)), 
                      pattern_color = after_scale(colorspace::darken(fill, 0.4))
                      ),
                  pattern_density = 0.1,
                  pattern_size = 0.1,
                  size = 0.2, color = "grey10") +
  geom_text_repel(aes(geometry = geometry, label = str_wrap(paste0(race, " ", type), 20)), size = 2.5, family = f1, stat = "sf_coordinates", seed = 1, bg.color = "#ffffff50", bg.r = 0.15, point.size = 0, segment.size = 0.2) +
  scale_fill_stepsn(colors = pal, breaks = seq(0, 16, 3)) +
  scale_pattern_discrete(name = "Race") +
  scale_pattern_angle_discrete(name = "Race") +
  scale_pattern_spacing_identity(name = "Race") +
  coord_sf(expand = FALSE) +
  guides(
    fill = guide_colorsteps(title.position = "top", order = 1, title = "LQ"),
    pattern = guide_legend(title.position = "top", label.position = "bottom", keywidth = 4.5, override.aes = list(pattern_spacing = c(0.08, 0.08, 0.08, 0.18, 0.08)))
    ) +
  labs(
    title = "Artist occupations with the highest concentration by state",
    subtitle = str_wrap("Location quotient (LQ) measures an artist occupation's concentration in the labor force, relative to the U.S. labor force share. The map shows the combination of race and occupation with the highest LQ in each state.", 140),
    caption = "Source: arts.gov · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.margin = margin(20, 10, 0, 10),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.title = element_text(face = "bold", color = "grey20"),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(lineheight = 1),
    plot.margin = margin(15, 15, 15, 15)
  )
