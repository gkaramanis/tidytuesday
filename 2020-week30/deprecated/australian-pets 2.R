library(tidyverse)
library(janitor)
library(lubridate)
library(ggforce)
# library(sf)

animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv') %>% 
  clean_names()

# # Read in shapefile for suburbs and union to get boundaries of city
# subs <- st_read("/Users/georgios/Documents/Projects/R/tidytuesday/2020-week30/data/TCC_Suburbs-shp/TCC_Suburbs.shp") %>% 
#   st_union() %>% 
#   st_set_crs(4326)

# # Read in shapefile for electoral divisions (some go into the sea)
# divs <- st_read("/Users/georgios/Documents/Projects/R/tidytuesday/2020-week30/data/Townsville_Final_Divisions-ESRI/Townsville_City_Divisions.shp") %>% 
#   st_as_sf() %>% 
#   st_transform(4326)

# # Get intersection of electoral divisions and city boundaries
# divs_clipped <- st_intersection(divs, subs) %>% 
#   select(electoral_division = Division)
# # Create centroids of clipped electoral divisions
# divs_centroid <- st_centroid(divs_clipped) 

# Read in population data
population <- read_csv(here::here("2020-week30", "data", "population.csv")) %>% 
  set_names(c("area", "population"))

noisy <- animal_complaints %>% 
  filter(complaint_type == "Noise") %>%
  select(-suburb) %>%
  left_join(population, by = c("electoral_division" = "area")) %>% 
  mutate(
    date_received = as.Date(parse_date_time(date_received, orders = c("m Y"))),
    year = year(date_received),
		month = month(date_received)
    ) %>%
  group_by(electoral_division, year) %>% 
  mutate(
    total = n(),
    pc = total / population,
		a = pc / max(noisy$pc, na.rm = TRUE) * 60
    ) %>% 
  ungroup() %>% 
  distinct(electoral_division, year, total, pc, a) %>%
	filter(year < 2020 & year > 2013) %>%
	filter(electoral_division != "Unallocated") %>% 
  group_by(electoral_division) %>% 
  mutate(
    div_x = 10 * ((cur_group_id() - 1) %% 5),
    div_y = 5 * (1 - cur_group_id() %/% 6)
    )

subs_list <- animal_complaints %>% 
  filter(complaint_type == "Noise") %>%
  filter(suburb != "Unallocated") %>% 
  mutate(
    date_received = as.Date(parse_date_time(date_received, orders = c("m Y"))),
    year = year(date_received),
    month = month(date_received)
  ) %>%
  group_by(suburb, year) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  filter(year < 2020 & year > 2013) %>%
  distinct(year, month, suburb, electoral_division, total) %>% 
  group_by(suburb) %>% 
  mutate(
    sub_x = 1 * ((cur_group_id() - 1) %% 6),
    sub_y = 1 * (10 - cur_group_id() %/% 7)
  ) %>% 
  ungroup()
	
  # left_join(divs_centroid) %>% 
  # rowwise() %>% 
  # mutate(
  #   x = geometry[[1]][[1]],
  #   y = geometry[[1]][[2]]
  #   ) %>% 
  # ungroup()

ggplot(noisy) +
  # geom_sf(data = divs_clipped) +
  # geom_sf(data = divs_centroid) +
  geom_arc(aes(x0 = div_x, y0 = div_y, r = (year - 2016.5) / 2, start = (90 - a) * pi/180, end = (90 + a) * pi/180, colour = total), size = 1.5) +
  coord_fixed() +
  geom_text(data = subs_list, aes(x = sub_x * 8, y = sub_y - 14, label = suburb), check_overlap = TRUE, family = "Proxima Nova", size = 2.5, hjust = 1) +
  # geom_tile(data = subs_list, aes(x = sub_x * 8 + month/3, y = sub_y - 14, fill = month_med, height = 0.3, width = 0.2)) +
  geom_line(data = subs_list, aes(x = sub_x * 8 -1006.5 + (year + month/12) / 2, y = sub_y - 14 + total/300, group = suburb, colour = total)) +
	# xlim(-5, 5) +
	# ylim(-5, 5) +
	# scale_colour_binned() +
  # facet_wrap(vars(electoral_division), ncol = 5) +
  #theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020-week30", "plots", "temp", paste0("australian-pets-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

 



