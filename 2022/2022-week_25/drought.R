library(tidyverse)
library(camcorder)
library(scico)

gg_record(dir = "tidytuesday-temp", device = "png", width = 16, height = 9, units = "in", dpi = 320)

# Read in drought data
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

# Shapefile from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us <- sf::read_sf(here::here("2022/2022-week_25/data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp")) %>% 
  janitor::clean_names() %>% 
  filter(!statefp %in% c("02", "15", "72")) %>% 
  mutate(fips = paste0(statefp, countyfp))

# Calculate mean DCSI by year and month, filter > 2012
month_fips <- drought_fips %>% 
  janitor::clean_names() %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE)
  ) %>% 
  group_by(fips, year, month) %>% 
  summarise(mean_dsci = mean(dsci)) %>%
  ungroup() %>% 
  filter(year > 2012)

# Combine with shapefile
fips_us <- us %>% 
  right_join(month_fips)

# Create US outline and combine with years and months
ym <- month_fips %>% 
  distinct(year, month)

us_outline <- sf::st_union(us) %>% 
  as.data.frame() %>% 
  bind_cols(ym)

# Fonts
f1 <- "Outfit"

# Plot, takes time!
ggplot(fips_us) +
  geom_sf(aes(fill = mean_dsci, geometry = geometry), color = NA) +
  geom_sf(data = us_outline, aes(geometry = geometry), fill = NA, size = 0.25) +
  scale_fill_scico(palette = "bilbao") +
  facet_grid(vars(year), vars(month)) +
  labs(
    title = "Drought Severity and Coverage Index",
    subtitle = str_wrap("The Drought Severity and Coverage Index is an experimental method for converting drought levels from the U.S. Drought Monitor map to a single value for an area. DSCI values are part of the U.S. Drought Monitor data tables. Possible values of the DSCI are from 0 to 500. Zero means that none of the area is abnormally dry or in drought, and 500 means that all of the area is in D4, exceptional drought.", 140),
    caption = "Source: U.S. Drought Monitor · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_colorbar(title = "Mean Monthly DSCI", title.position = "top")) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.85, 1.1),
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.title.align = 0.5,
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(0, 20, 0, 20),
    strip.text = element_text(margin = margin(0, 0, 2, 0), color = "grey40", size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(3, 0, 20, 0)),
    plot.caption = element_text(margin = margin(5, 0, 0, 0))
  )
  
