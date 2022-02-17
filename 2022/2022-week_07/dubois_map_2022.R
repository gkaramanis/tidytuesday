library(tidyverse)
library(camcorder)
library(rgeoboundaries)
library(sf)

gg_record(dir = "temp", device = "png", width = 7, height = 8, units = "in", dpi = 320)

usa_sf <- geoboundaries("USA")

usa <- usa_sf %>% 
  st_crop(c(xmin = -125, xmax = -67, ymin = 25, ymax = 50)) %>% 
  st_simplify(dTolerance = 100) %>% 
  st_transform("EPSG:2163")

pop <- data.frame(
  facet = rep(c(1800, 1830, 1860, 1890), each = 2),
  text = rep(c("one-fifth", "one-sixth", "one-seventh", "one-eighth"), each = 2),
  pop = rep(c("total", "black")),
  ratio = c(0.27, 0.10, 0.43, 0.14, 0.70, 0.21, 1, 0.30)
  )

usa_pop <- merge(usa, pop) %>% 
  rowwise() %>% 
  mutate(
    geometry = geometry * ratio + c(ratio * 100, ratio * 5*10^5),
    ymin = st_bbox(geometry)[2]
    )

f1 = "Jefferies"

ggplot() +
  geom_sf(data = usa_pop %>% filter(pop == "total"), fill = NA, color = "brown4") +
  geom_sf(data = usa_pop %>% filter(pop == "black"), fill = "grey10", size = 0) +
  geom_text(data = usa_pop %>% filter(pop == "total"), aes(0, ymin - 3*10^5, label = toupper(text), size = ratio), family = f1, color = "#035403") +
  geom_text(data = usa_pop %>% filter(pop == "total"), aes(0, ymin + ratio * 3* 10^6, label = facet, size = ratio), family = f1, color = "grey10") +
  scale_fill_identity() +
  scale_size_continuous(range = c(3, 9)) +
  facet_wrap(vars(facet)) +
  labs(
    title = "PROPORTION OF NEGROES IN THE TOTAL POPULATION OF THE UNITED STATES .\n\nRAPPORT DES NÈCRES A LA POPULATION TOTALE DES ETATS UNIS .\n\nDONE BY ATLANTA UNIVERSITY .",
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.title = element_text(hjust = 0.5, family = f1, margin = margin(0, 0, 60, 0), size = 15),
    plot.caption = element_text(size = 9, family = f1, hjust = 0.5, margin = margin(30, 0, -20, 0)),
    plot.background = element_rect(fill = "#C6BFB5", color = NA),
    plot.margin = margin(0, 10, 0, 0)
  )

