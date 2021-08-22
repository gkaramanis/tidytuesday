# Packages loaded in australian-pets.R

# Read in shapefile for suburbs and union to get boundaries of city
subs <- st_read("/Users/georgios/Documents/Projects/R/tidytuesday/2020-week30/data/TCC_Suburbs-shp/TCC_Suburbs.shp") %>% 
  st_union() %>% 
  st_set_crs(4326)

# Read in shapefile for electoral divisions (some go into the sea)
divs <- st_read("/Users/georgios/Documents/Projects/R/tidytuesday/2020-week30/data/Townsville_Final_Divisions-ESRI/Townsville_City_Divisions.shp") %>% 
  st_as_sf() %>% 
  st_transform(4326)

# Get intersection of electoral divisions and city boundaries
divs_clipped <- st_intersection(divs, subs) %>% 
  clean_names() %>% 
  mutate(
    facet = case_when(
      division_id %in% c(1, 2, 5, 7, 8) ~ "left",
      TRUE ~ "right"
    ))

# Create centroids of clipped electoral divisions
divs_centroid <- st_centroid(divs_clipped) %>% 
  clean_names()

map <- ggplot(divs_clipped) +
  geom_sf(fill = pal[6], colour = pal[7], size = 0.25, alpha = 0.6) +
  geom_sf_text(aes(label = division_id), size = 3, family = "Produkt", colour = "grey10") +
  annotate("text", 146.3, -19.4, label = "Townsville\nelectoral divisions", hjust = 0, family = "Produkt", size = 4, lineheight = 0.9) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.background = element_rect(fill = pal[7], colour = NA),
    plot.margin = margin(130, 120, 130, 120)
    )

# map
