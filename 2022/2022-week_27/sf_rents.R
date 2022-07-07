library(tidyverse)
library(camcorder)
library(sf)
library(osmdata)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 10, units = "in", dpi = 320)

# Inspiration and code by Philippe Massicotte
# https://github.com/PMassicotte/tidytuesday/blob/master/R/tidytuesday_2020_week05.R

# Read in data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# Read in SF blocks shapefile 
san_f <- read_sf(here::here("2022/2022-week_27/data/sf_neighborhoods/geo_export_42d6e0d7-4017-4b4c-bb21-1900c149f2ae.shp"))
  
# Get bounding box of SF
bb <- st_bbox(san_f)

# Get streets of SF
roads <- st_bbox(san_f) %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

# Transform streets and intersect
roads2 <- roads$osm_lines %>%
  st_transform(st_crs(san_f)) %>%
  st_intersection(san_f)

# Calculate price per sqft for each listing, transform to spatial and intersect
rent_bb <- rent %>% 
  mutate(price_per_sqft = price / sqft) %>% 
  drop_na(lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(st_crs(san_f)) %>% 
  st_intersection(san_f)

# Join SF blocks and rents, calculate median rent by block
rent_sf <- san_f %>% 
  st_join(rent_bb) %>% 
  group_by(name.x) %>% 
  summarize(
    price_sqft = median(price / sqft, na.rm = TRUE)
  ) %>% 
  ungroup()

# Palettes and fonts
pal <- scico::scico(palette = "imola", direction = -1, n = 5)

f1 <- "Outfit"

# Plot
ggplot() +
  # Price per sqft
  geom_sf(data = rent_sf, aes(fill = price_sqft), size = 0.1, color = NA) +
  # Streets
  geom_sf(data = roads2, size = 0.1, alpha = 0.4, color = "grey20") +
  # All Craigslist posts
  geom_sf(data = rent_bb, aes(size = price), shape = 21, fill = "lightsalmon2", stroke = 0.25, color = "brown4") +
  scale_fill_stepsn(colors = pal, na.value = "grey85", show.limits = TRUE, guide = guide_coloursteps(title = "Median price per square foot ($)", title.position = "top")) +
  scale_size_binned_area(labels = scales::number_format(big.mark = ","), guide = guide_bins(title = "Price ($)", title.position = "top", show.limits = TRUE, order = 1)) +
  coord_sf(expand = FALSE, clip = "off", ylim = c(37.71, 37.82)) +
  labs(
    title = "San Francisco rent prices",
    subtitle = paste0("From ", nrow(rent_bb), " Craigslist posts between ", min(rent_bb$year), " and ", max(rent_bb$year), ". Price per square foot calculated where available"),
    caption = str_wrap("Source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018. Retrieved from https://github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip · Graphic: Georgios Karamanis", 150)
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey96", color = NA),
    legend.position = c(0.22, 1.05),
    legend.text = element_text(colour = "grey40"),
    legend.title = element_text(colour = "brown", face = "bold"),
    legend.direction = "horizontal",
    legend.key.width = unit(3.5, "line"),
    legend.key.height = unit(0.5, "line"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 20, face = "bold", color = "brown"),
    plot.subtitle = element_text(size = 13, margin = margin(7, 0, 100, 0), color = "grey30"),
    plot.caption = element_text(size = 9, color = "grey40", margin = margin(13, 0, 0, 0))
  )
