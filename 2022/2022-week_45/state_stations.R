library(tidyverse)
library(geofacet)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

stations <- state_stations %>% 
  mutate(
    frequency = parse_number(frequency),
    band = if_else(frequency <= 108, "FM", "AM"),
    genre = case_when(
      str_detect(tolower(format), "adult contemporary") ~ "adult contemporary",
      str_detect(tolower(format), "public radio") ~ "public radio"
      ),
    state = str_replace(state, "_", " ")
    ) %>% 
  filter(band == "FM" & !is.na(genre))

f1 <- "Outfit"

freq_lab <- data.frame(x = seq(88, 108, 5))

ggplot(stations) +
  geom_segment(aes(x = 87.5, xend = 108.5, y = -0.1, yend = -0.1), size = 0.2, stat = "unique") +
  geom_text(data = freq_lab, aes(x = x, y = -0.5, label = x), vjust = 1, size = 1.5, family = f1, color = "#23466E90") +
  geom_histogram(aes(x = frequency, fill = genre), binwidth = 1) +
  scale_fill_manual(values = c("#E5943E", "#724615")) +
  coord_cartesian(clip = "off") +
  facet_geo(vars(state), strip.position = "bottom") +
  labs(
    title = "Public radio is on the left",
    subtitle = "Distribution of FM frequencies for<br><span style='color:#724615'>**public radio**</span> and <span style='color:#E5943E'>**adult contemporary**</span> stations",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    strip.clip = "off", # ggplot 3.4.0
    strip.text = element_text(face = "bold", margin = margin(3, 0, 0, 0)),
    plot.background = element_rect(fill = "#F5F9FD", color = NA),
    plot.margin = margin(20, 20, 10, 20),
    plot.title = element_text(face = "bold", size = 20, margin = margin(0, 0, -50, 0), color = "#23466E"),
    plot.subtitle = element_markdown(margin = margin(55, 0, -90, 0), lineheight = 1.2, size = 12),
    plot.caption = element_text(margin = margin(20, 0, 0, 0), color = "#23466E")
  )
  
record_polaroid()
