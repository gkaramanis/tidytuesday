library(tidyverse)
library(camcorder)
library(ggbump)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 6, units = "in", dpi = 320)

# Read in flights
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>% 
  janitor::clean_names()
  
# Read in Greece shapefile
gr <- sf::read_sf(here::here("2022/2022-week_28/data/gadm36_GRC_0.shp"))

# Read in airport data
airports <- read_csv("https://raw.githubusercontent.com/ip2location/ip2location-iata-icao/master/iata-icao.csv")

# Years to compare
y1 <- 2019
y2 <- 2022

flt <- flights %>% 
  filter(state_name == "Greece") %>%
  filter(year == y1 | year == y2) %>%
  group_by(year, month_num, apt_icao) %>% 
  summarise(
    # Sum monthly arrivals
    arr_month = sum(flt_arr_1), 
    apt_icao,
    apt_name,
    year,
    month_mon
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(airports, by = c("apt_icao" = "icao")) %>% 
  group_by(apt_icao) %>% 
  mutate(
    month = as.numeric(month_num),
    # create y for airports for drawing, remap range 1-13 to 35-43 
    apt_y = ((cur_group_id() - 1) * (43 - 35)) / (13-1) + 35 - month/12, # Divide by 12 for spacing between lines
    s = if_else(longitude > 24, 1, -1) # assign +/- sign for placing lines to right/left
  ) %>% 
  ungroup() 

# Monthly totals for all airports 
arr_y2 <- flt %>% 
  filter(year == y2)
  
arr_y2_all <- sum(arr_y2$arr_month)

arr_y1 <- flt %>% 
  filter(year == y1) %>% 
  filter(month < 6)

arr_y1_all <- sum(arr_y1$arr_month)

tot_arr_y2 <- arr_y2 %>% 
  group_by(month) %>% 
  summarise(arr_tot = sum(arr_month), month_mon) %>% 
  ungroup()

tot_arr_y1 <- arr_y1 %>% 
  group_by(month) %>% 
  summarise(arr_tot = sum(arr_month), month_mon) %>% 
  ungroup()

# Fonts
f1 <- "Input Mono Narrow"
f2 <- "Messapia"
f3 <- "Outfit"

# Plots
ggplot(arr_y1) +
  geom_sf(data = gr, color = NA) +
  # City labels
  geom_text(aes(x = longitude, y = latitude, label = apt_name, fill = apt_icao, color = after_scale(colorspace::darken(fill, 0.5))), stat = "unique", nudge_y = -0.2, size = 3, family = f3) + # geom_text does not have fill but use it here so that after_scale can be used for color
  geom_point(aes(x = longitude, y = latitude, color = apt_icao), stat = "unique") +
  # Lines for 2020
  # Sigmoid from airport to right/left of map
  geom_sigmoid(data = arr_y2, aes(x = longitude, y = latitude, xend = 24 + s * 6, yend = apt_y, color = apt_icao), size = 0.3) +
  # Lines for actual numbers
  geom_segment(data = arr_y2, aes(x = (24 + s * 6.16) + s * 0.025, y = apt_y, xend = (24 + s * 6.16) + s * 0.025 + s * arr_month/5000, yend = apt_y, color = apt_icao), size = 0.3) +
  # 2019 indicator
  geom_tile(data = arr_y1, aes(x = (24 + s * 6.16) + s * 0.025 + s * arr_month/5000, y = apt_y, fill = apt_icao, width = 0.015, height = 0.05)) +
  # Month labels
  geom_text(aes(x = 24 + s * 6.09, y = apt_y, color = apt_icao, label = month_mon), size = 1, hjust = 0.5, family = f1, stat = "unique") +
  # Title, subtitle
  annotate("text", x = 14.5, y = 42.7, label = "ARRIVALS", size = 5.5, fontface = "bold", family = f2) +
  annotate("text", x = 14.5, y = 42.2, label = y2, size = 10, fontface = "bold", family = f2) +
  annotate("text", x = 14.5, y = 41.65, label = "GREECE", size = 6.3, fontface = "bold", family = f2) +
  annotate("text", x = 13, y = 41.2, label = str_wrap(paste0("Arrivals to Greek airports in 2022 are at pre-pandemic levels. From January to April 2022 a total of ", scales::number(arr_y2_all), " flights arrived to Greece, compared to ", scales::number(arr_y1_all), " flights during the same period in 2019."), 29), size = 3,  family = f3, lineheight = 0.95, hjust = 0, vjust = 1, color = "grey20") +
  # All arrivals chart below subtitle
  geom_text(data = tot_arr_y2, aes(x = 13, y = 39 - month/4, label = month_mon), hjust = 0, size = 2.5, family = f1, color = "grey25") +
  geom_segment(data = tot_arr_y2, aes(x = 13.45, y = 39 - month/4, xend = 13 + arr_tot/8000, yend = 39 - month/4), size = 0.5, color = "grey25") +
  geom_tile(data = tot_arr_y1, aes(x = 13 + arr_tot/8000, y = 39 - month/4, width = 0.04, height = 0.1), fill = "grey25") +
  annotate("text", x = 13, y = 37.2, label = "Arrivals to all Greek airports\nin 2022 (horizontal lines) and\n2019 (vertical lines)", hjust = 0, vjust = 1, family = f3, size = 2.5, lineheight = 0.95) +
  # Caption
  annotate("text", x = 13, y = 34.4, label = "Source: Eurocontrol · Graphic: Georgios Karamanis", hjust = 0, vjust = 1, family = f3, size = 2, color = "grey40") +
  # Scales, etc
  scale_color_manual(values = colorspace::darken(c('#e6194B', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#000000', '#800000', "#000075"))) +
  scale_fill_manual(values = colorspace::darken(c('#e6194B', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#000000', '#800000', "#000075"))) +
  coord_sf(xlim = c(14, 30.5), clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
