library(tidyverse)
library(camcorder)
library(sf)
library(ggpattern)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

us_sf <- rgeoboundaries::gb_adm0("usa") %>% 
  st_transform(crs = 5070)

bear_sf <- read_sf(here::here("2022/2022-week_37/data/mABBEx_CONUS_Range_2001v1/mABBEx_CONUS_Range_2001v1.shp"))
  
bbox <- st_bbox(bear_sf)

bear_label <- data.frame()

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv') %>% 
  mutate(
    name = case_when(
      str_detect(tolower(observed), "bigfoot") ~ "Bigfoot",
      str_detect(tolower(observed), "sasquatch") ~ "Sasquatch",
      str_detect(tolower(observed), "skunk ape") ~ "Skunk ape",
      # str_detect(tolower(observed), "bear(s)*\\b") ~ "Bear is mentioned",
      # str_detect(tolower(observed), "monster") ~ "Monster",
      TRUE ~ "Other name"
    ),
    name = fct_relevel(name, c("Bigfoot", "Sasquatch", "Skunk ape", "Other name or none"))
  )

bigfoot_sf <- bigfoot %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 5070)

f1 <- "Outfit"
f2 <- "Borsok"

pal <- chroma::mix(rev(RColorBrewer::brewer.pal(4, name = "Dark2")), "brown", 0.5)

ggplot() +
  geom_sf(data = us_sf, size = 0.1, fill = "antiquewhite") +
  geom_sf_pattern(data = bear_sf, pattern = "magick", pattern_fill = "brown", fill = "antiquewhite2",  size = 0.1, color = NA) +
  geom_sf(data = bigfoot_sf, aes(color = name), size = 0.7, alpha = 0.85) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  scale_color_manual(values = pal) +
  facet_wrap(vars(name)) +
  labs(
    title = "Bigfoot or Bear?",
    subtitle = str_wrap("Locations of more than 4,000 geocoded Bigfoot sighting reports from 1869 to 2021, broken down by the name that appears in the report. The shaded area shows the known range of the American black bear (USGS, 2001).", 110),
    caption = "Source: Bigfoot Field Researchers Organization & USGS · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_text(size = 14, margin = margin(0, 0, 1, 0), face = "bold", color = "darkorange4"),
    plot.title = element_text(size = 28, family = f2, color = "darkorange4"),
    plot.subtitle = element_text(size = 14, margin = margin(10, 0, 30, 0), lineheight = 1, color = "gray10"),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), color = "grey40"),
    plot.margin = margin(10, 15, 5, 15)
  )
  


