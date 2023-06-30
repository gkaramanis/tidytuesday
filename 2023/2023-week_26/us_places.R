library(tidyverse)
library(sf)
library(glue)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')

# https://www.sciencebase.gov/catalog/item/4fb55df0e4b04cb937751e02
us_water <- read_sf(here::here("2023/2023-week_26/data/NA_Lakes_and_Rivers/"))

us_beach <- us_place_names %>% 
  filter(str_detect(tolower(feature_name), "beach")) %>% 
  filter(!is.na(prim_long_dec)) %>% 
  st_as_sf(coords = c("prim_long_dec", "prim_lat_dec"), crs = 4326) %>% 
  st_transform(crs = st_crs(us_water))

us_sf <- rgeoboundaries::gb_adm1("usa") %>% 
  st_transform(crs = st_crs(us_water))

us_inter <- st_intersection(us_water, us_sf)

coast <- rnaturalearthdata::countries50 %>% 
  st_as_sf()

na <- rgeoboundaries::gb_adm0(c("united states", "canada", "mexico", "greenland", "bahamas", "cuba", "russia", "iceland"))

cities <- read_csv("https://gist.githubusercontent.com/randysecrist/2d28eb67880ca3cc6fd8c6f8e0b1422a/raw/a9f316b2c80956cf91824d79915c2a1713ca47de/cities.py", skip = 3)

cities_lab <- us_beach %>% 
  inner_join(cities, by = c("feature_name" = "city", "state_name" = "state")) %>% 
  filter(as.numeric(rank) < 501)

beach_count <- us_beach %>% 
  st_drop_geometry() %>% 
  count(state_name, sort = TRUE) %>% 
  mutate(
    total = sum(n),
    pct = round(n / total * 100, 1)
    )

f1 <- "Outfit"
f2 <- "Publico Headline"
f3 <- "DIN Condensed"

ggplot(us_beach) +
  geom_sf(data = na, linewidth = 0.1, fill = "grey97", color = NA) +
  geom_sf(data = us_inter, color = "#BFC9E0", linewidth = 0.4) +
  geom_sf(data = us_sf, linewidth = 0.1, fill = "NA", color = "black") +
  geom_sf(size = 2, shape = 4, color = "#E33968", stroke = 0.5, alpha = 0.9) +
  # Biggest cities
  ggrepel::geom_text_repel(data = cities_lab, aes(geometry = geometry, label = feature_name), stat = "sf_coordinates", size = 3, family = f3, seed = 100, min.segment.length = 0, segment.size = 0.3, bg.color = "white", bg.r = 0.05) +
  # Title
  annotate("richtext", -47e5, 20e5, label = "Populated places<br>containing the<br>word <span style='color:#E33968'>beach</span>", family = f2, size = 10, hjust = 0, vjust = 1, lineheight = 1.1, fontface = "bold", fill = NA, label.color = NA, label.padding = unit(c(0, 0, 0, 0), "lines")) +
  # Subtitle
  annotate("text", -47e5, 8.5e5, label = glue("{beach_count$n[1]} of the {scales::number(beach_count$total[1])} places are in\n{beach_count$state_name[1]} and {beach_count$n[2]} in {beach_count$state_name[2]}"), family = f2, size = 6, hjust = 0, vjust = 1) +
  # Caption
  annotate("text", -47e5, -20.5e5, label = "Source: USGS · Graphic: Georgios Karamanis", family = f1, size = 3.5, hjust = 0, vjust = 1) +
  coord_sf(xlim = c(-45e5, 25e5), ylim = c(-20e5, 38e5)) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#BFC9E0", color = NA)
  )
