library(tidyverse)
library(sf)
library(ggrepel)
library(tigris)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')

rifle <- longbeach %>% 
  filter(animal_id == "A637086") %>% 
  arrange(intake_date) %>% 
  mutate(
    age = round((intake_date - dob )/ 365.25),
    intake_date_label = stamp("March 1, 2000")(intake_date),
    intake_year = year(intake_date),
    i = row_number(),
    crossing = crossing %>%
      str_remove(",* LONG BEACH.+") %>%
      str_remove(" LB [0-9]{5}") %>%
      str_replace("([0-9]+) BLK ", "\\1 "),
    outcome_subtype = case_when(
      outcome_subtype == "fre rid hm" ~ "free ride home",
      outcome_subtype == "walkin" ~ "walk-in",
      TRUE ~ outcome_subtype
    )
  )

# Create a bounding box from the coordinates
bbox <- c(
  xmin = min(rifle$longitude) * 1.0001,
  xmax = max(rifle$longitude) * 0.9999,
  ymin = min(rifle$latitude) * 0.9985,
  ymax = max(rifle$latitude) * 1.0006
)

# Convert to sf bbox and the to polygon for clipping
bbox_polygon <- st_bbox(bbox) %>%
  st_as_sfc()

la_county <- counties("California", resolution = "500k", cb = TRUE) %>% 
  filter(str_detect(NAME, "Los Angeles")) %>% 
  `st_crs<-`(., st_crs(bbox_polygon)) %>% 
  st_intersection(bbox_polygon)

lb_roads <- roads("California", "Los Angeles County") %>% 
  `st_crs<-`(., st_crs(bbox_polygon)) %>% 
  st_intersection(bbox_polygon) %>% 
  rmapshaper::ms_simplify() 

f1 <- "Shantell Sans Informal"
f2 <- "American Typewriter"

timeline <- data.frame(x = seq.Date(min(rifle$intake_date), max(rifle$intake_date), by = "1 month"))

t <- ggplot(rifle, aes(x = intake_date, y = 0)) +
  geom_point(data = timeline, aes(x = x, y = 0), color = "grey50", shape = "|", size = 2) +
  geom_label_repel(aes(label = paste0(intake_date_label, "\nage ", age)), family = f1, lineheight = 0.95, direction = "y", min.segment.length = 0, seed = 999, force_pull = 0, nudge_y = c(0.1, 0.1, 0.1, 0.1, 0.1, -0.1, 0.1, 0.1), box.padding = 0.5, label.padding = 0.5, label.size = 0, fill = NA, segment.color = "#4A4238") +
  geom_point(size = 5, color = "#4A4238") +
  geom_text(aes(label = i), color = "white", size = 3) +
  scale_x_date(expand = c(0.1, 0.1)) +
  scale_y_continuous(limits = c(-0.15, 0.2)) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "cornsilk2", color = "black"),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(0, 20, 0, 20)
  )
  
m <- ggplot() +
  geom_sf(data = la_county, fill = "cornsilk2", color = NA) +
  geom_sf(data = lb_roads, color = "tan", linewidth = 0.2) + 
  geom_point(data = rifle, aes(longitude, latitude), size = 4.5, color = "#4A4238") +
  geom_text(data = rifle, aes(longitude, latitude, label = i), size = 3, color = "white") +
  geom_text_repel(data = rifle, aes(x = longitude, y = latitude, label = paste0(crossing, "\n", outcome_type, "/", outcome_subtype)), family = f1, lineheight = 0.95, size = 3, seed = 999, point.padding = 2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  facet_wrap(vars(intake_year), nrow = 1) +
  labs(
    title = "The Adventurous Life of Rifle: Long Beach's Most Frequent Runaway",
    subtitle = "Since 2019, this tan and black dog has been found wandering the streets of Long Beach\neight different times, always making his way back home to his patient owner",
    caption = "Source: City of Long Beach Animal Care Services · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    panel.border = element_rect(fill = NA, color = "black"),
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = "#c7d9e0", color = NA),
    strip.text = element_text(margin = margin(0, 0, 5, 0), size = 12),
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(15, 0, 0, 0), face = "bold", color = "#4A4238"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(5, 0, 15, 0), family = f1, color = "#414A4C")
  )

t / m +
  plot_layout(heights = c(1, 1.5))
