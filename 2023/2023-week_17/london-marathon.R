library(tidyverse)
library(sf)
library(lwgeom)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')

route <- read_sf(here::here("2023/2023-week_17/data/london-marathon.geojson")) %>% 
  janitor::clean_names() %>% 
  mutate(length = st_length(geometry)) %>% 
  filter(str_detect(name, "London|2"))
  
main_route <- route %>% 
  filter(name == "London Marathon Course")

# https://stackoverflow.com/questions/67987924/find-coordinates-x-distance-along-linestring
winners_p <- winners %>% 
  janitor::clean_names() %>% 
  filter(category == "Women") %>%
  group_by(category) %>% 
  arrange(time) %>% 
  mutate(rank = row_number()) %>% 
  # filter(rank < 11) %>% 
  mutate(
    time_diff = time - min(time),
    ratio = scales::rescale(as.numeric(time_diff), to = c(1, 0))
    ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(point = st_linesubstring(main_route, from = 0, to = ratio) %>% st_endpoint()) %>% 
  ungroup()

# route_bbox <- st_bbox(route)

f1 <- "Saira"

ggplot() +
  geom_sf(data = route, aes(geometry = geometry), color = "cornflowerblue", linewidth = 2.5) +
  geom_sf(data = route, aes(geometry = geometry), color = "white", linewidth = 0.2) +
  geom_sf(data = winners_p, aes(geometry = point, size = rank < 11), fill = "#D13166", color = "white", shape = 21) +
  ggrepel::geom_label_repel(data = winners_p %>% filter(rank < 11), aes(geometry = point, label = paste0(rank, ". ", athlete, "\n", nationality, " ", time, " (", year, ")")), family = f1, stat = "sf_coordinates", direction = "y", nudge_y = 0.032, size = 3.5, segment.size = 0.15, label.size = 0.15, seed = 999) +
  annotate("text", x = 0.01, y = 51.547, label = str_wrap("This visualization shows all the women's winners of the London Marathon, represented as dots along the route of the race. The distance between the dots is proportional to the difference in their finishing times (but it does not represent the actual distance that could be covered in the corresponding amount of time). Highlighted are the 10 fastest winners.", 35), hjust = 0, vjust = 1, family = f1) +
  scale_size_manual(values = c(2.5, 4)) +
  coord_sf(xlim = c(-0.15, 0.06)) +
  labs(
    title = "Mapping the Victories: Women's Winners of the London Marathon",
    caption = "Source: LondonMarathon R package · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#EEF2F6", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24, margin = margin(15, 0, 30, 0), color = "#D13166"),
    plot.caption = element_text(hjust = 0.5, margin = margin(5, 0, 10, 0))
  )
  
