library(tidyverse)
library(scales)
library(cowplot)
library(ggforce)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
# eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

world_map <- map_data("world")

volcano_map <- volcano %>% 
  mutate(primary_volcano_type = str_remove(primary_volcano_type, "\\(.+\\)|\\?"))

map_plot <- ggplot(volcano_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey30", colour = "grey45", size = 0.1) +
  geom_mark_circle(aes(longitude, latitude, label = volcano_name, description = "Highest Volcano, 6079 m", filter = elevation > 6870), label.fontsize = 6, label.fill = "grey45", label.colour = "grey90", label.lineheight = 0.9, label.margin = margin(1, 1, 1, 1, "mm"), con.size = 0.25, size = 0, expand = unit(0, "mm"),  con.cap = 0, con.colour = "grey95") +
  geom_mark_circle(aes(longitude, latitude, label = volcano_name, description = "Deepest Volcano, -2500 m", filter = elevation < -2499), label.fontsize = 6, label.fill = "grey45", label.colour = "grey90", label.lineheight = 0.9, label.margin = margin(1, 1, 1, 1, "mm"), con.size = 0.25, size = 0, expand = unit(0, "mm"),  con.cap = 0, con.colour = "grey95") +
  geom_point(aes(longitude, latitude, colour = primary_volcano_type), size = 0.5, shape = 17, alpha = 0.8) +
  scale_colour_viridis_d(option = "magma", guide = guide_legend(title = "Primary Volcano Type", nrow = 6, title.position = "top", keyheight = 0.7, keywidth = 0.8, override.aes = list(size = 1.2))) +
  coord_fixed() +
  labs(caption = "Volcano Eruptions | Source: The Smithsonian Institution | Graphic: Georgios Karamanis") +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    plot.background = element_rect(fill = "grey45", colour = "grey45"),
    legend.position = c(0.12, 0.22),
    legend.text = element_text(colour = "grey90", size = 6),
    legend.title = element_text(hjust = 0.02, colour = "grey90", size = 6, margin = margin(0, 0, 5, 0)),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(-0.1, 'lines'),
    plot.caption = element_text(colour = "grey90", size = 5, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

major_rocks <- volcano %>% 
  pivot_longer(cols = major_rock_1:major_rock_5, names_to = "major_rock", values_to = "major_rock_value") %>% 
  mutate(major_rock_value = ifelse(nchar(major_rock_value) == 1, NA, major_rock_value)) %>% 
  drop_na() %>% 
  group_by(major_rock_value) %>% 
  summarise(n = n()) %>% 
  mutate(
    freq = n / sum(n),
    major_rock_value = str_replace(major_rock_value, " / ", "\n")
    ) %>% 
  top_n(5, freq) %>% 
  mutate(major_rock_value = fct_reorder(major_rock_value, freq))

major_rocks_plot <- ggplot(major_rocks) +
  geom_col(aes(x = freq, y = major_rock_value), fill = "grey90", width = 0.6) +
  geom_text(aes(x = freq, y = major_rock_value, label = percent(freq, accuracy = 0.1)), hjust = 0, nudge_x = 0.01, family = "IBM Plex Mono", size = 2, colour = "grey90") +
  geom_text(aes(x = freq, y = major_rock_value, label = major_rock_value), hjust = 0, vjust = 0.5, family = "IBM Plex Mono", size = 1.8, colour = "grey90", nudge_x = 0.12, lineheight = 0.9) +
  xlim(0, 1) +
  labs(title = toupper("Most common major rocks")) +
  theme_void(base_family = "IBM Plex Sans", base_size = 5) +
  theme(
    plot.title = element_text(colour = "grey90", family = "IBM Plex Sans Bold", hjust = 0.08, margin = margin(0, 0, 2, 0))
  )

countries_list <- volcano %>% 
  count(country, sort = TRUE) %>% 
  top_n(10, n)

countries_list_plot <- ggplot(countries_list) +
  geom_text(aes(x = 0, y = country, label = paste0(country, " ", n)), family = "IBM Plex Sans", size = 2, hjust = 1, colour = "grey90") +
  labs(title = toupper("Countries with\nthe most volcanos")) +
  xlim(-1, 0) +
  theme_void(base_size = 5) +
  theme(
    plot.title = element_text(colour = "grey90", family = "IBM Plex Sans Bold", hjust = 0.93, margin = margin(0, 0, 3, 0))
    )

ggdraw(map_plot) +
  draw_plot(major_rocks_plot, x = 0.005, y = 0.54, height = 0.2, width = 0.3) +
  draw_plot(countries_list_plot, x = 0.79, y = 0.47, height = 0.24, width = 0.2) 

ggsave(here::here("2020-week20", "plots", "temp", paste0("volcano-eruptions-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 5.9)

