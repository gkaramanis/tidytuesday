library(tidyverse)
library(ggimage)
library(camcorder)
library(wesanderson)

gg_record(dir = "temp", device = "png", width = 12, height = 13.5, units = "in", dpi = 320)

drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')

driver_first <- drivers %>% 
  left_join(results, by = "driverId") %>% 
  left_join(races, by = "raceId") %>% 
  group_by(driverId) %>% 
  mutate(
    min_year = min(year),
    max_year = max(year),
    diff_years = max_year - min_year
    ) %>% 
  ungroup() %>% 
  filter(position == "1") 

  
driver_results <- driver_first %>% 
  count(driverId, forename, surname, diff_years) %>% 
  arrange(-n) %>% 
  mutate(
    full_name = paste(forename, surname),
    full_name = fct_reorder(full_name, n),
    i = row_number()
    ) %>% 
  head(20)

first_years <- driver_first %>% 
  count(driverId, year, min_year, max_year, name = "ny") %>% 
  mutate(diff_by_year = max_year - year) %>% 
  left_join(driver_results) %>% 
  filter(!is.na(n))

races_year <- races %>% 
  count(year) %>% 
  mutate(r = row_number())

f1 = "Piazzolla"
f2 = "Input Mono Compressed"
f2b = "Input Mono"

pal <- wes_palette("Zissou1")[-4]

ggplot(first_years) +
  # before car
  # Career length
  geom_segment(aes(x = n - 3, xend = n - diff_years - 3, y = i + 0.2, yend = i + 0.2), size = 1.5, color = "grey25") +
  geom_segment(aes(x = n - 3, xend = n - diff_years - 3, y = i - 0.2, yend = i - 0.2), size = 1.5, color = "grey25") +
  # Wins by year
  geom_tile(aes(x = n - diff_by_year - 3, y = i, width = ny/max(first_years$ny), fill = year), height = 0.5, color = NA, alpha = 0.9) +
  # Car
  geom_image(aes(x = n + 3, y = i, image = here::here("2021", "2021-week36", "img", "racing-car.png")), asp = 0.6, size = 0.07) +
  # after car
  # Wins
  geom_text(aes(x = n + 10, y = i, label = n), hjust = 0, size = 10, family = f2, fontface = "bold.italic", color = "grey97") +
  # Name
  geom_text(aes(x = n + 17, y = i - 0.14, label = full_name), hjust = 0, size = 5, family = f1, fontface = "bold", color = "grey95") +
  # Years
  geom_text(aes(x = n + 17, y = i + 0.19, label = paste(diff_years, "years  (", min_year, "-", max_year, ")")), hjust = 0, size = 4.5, family = f1, color = "grey70") +
  # Total races by year
  geom_segment(data = races_year, aes(x = 120, xend = 120 + n/1.8, y = r/4.8 + 5, yend = r/4.8 + 5, color = year), size = 2) +
  geom_text(data = races_year %>% filter(year %% 5 == 0), aes(x = 120, y = r/4.8 + 5, label = year, color = year), hjust = 1, nudge_x = -1, family = f1, fontface = "bold", size = 5) +
  geom_text(data = races_year %>% filter(year %% 5 == 0), aes(x = 120 + n/1.8, y = r/4.8 + 5, label = n, color = year), nudge_x = 2, family = f1, fontface = "bold") +
  # Annotations
  # total wins
  annotate("text", x = 52, y = 2, label = "total wins", hjust = 1, family = f1, color = "grey80", size = 5) +
  annotate("curve", x = 54, y = 2, xend = 63, yend = 2.5, arrow = arrow(length = unit(0.01, "npc")), color = "grey60", curvature = -0.3) +
  # career (text)
  annotate("text", x = 115, y = 3, label = "years active", hjust = 0, family = f1, color = "grey80", size = 5) +
  annotate("curve", x = 113, y = 3.1, xend = 96, yend = 3.2, arrow = arrow(length = unit(0.01, "npc")), color = "grey60", curvature = -0.1) +
  # wins by year
  annotate("text", x = 20, y = 2.9, label = "thickness of\ncolor lines shows\nnumber of wins\nby season", hjust = 1, family = f1, color = "grey80", size = 5, lineheight = 0.9, vjust = 0) +
  annotate("curve", x = 22, y = 2.9, xend = 33, yend = 3, arrow = arrow(length = unit(0.01, "npc")), color = "grey60", curvature = 0.1) +
  # career (lines)
  annotate("text", x = 20, y = 5, label = "gray lines show\ncareer length", hjust = 1, family = f1, color = "grey80", size = 5, lineheight = 0.9, vjust = 0) +
  annotate("curve", x = 22, y = 5, xend = 25, yend = 5.6, arrow = arrow(length = unit(0.01, "npc")), color = "grey60", curvature = -0.15) +
  # year legend
  annotate("text", x = 108, y = 20, label = "Number of\nraces by season", hjust = 1, family = f1, color = "grey80", size = 5, lineheight = 0.9, vjust = 0) +
  # Title, subtitle, caption
  labs(
    title = "Top 20 Formula One drivers",
    subtitle = "Source: Ergast API · Graphic: Georgios Karamanis"
  ) +
  # Scales, theme, etc.
  scale_color_gradientn(colors = pal) +
  scale_fill_gradientn(colors = pal) +
  scale_x_continuous(limits = c(-10, 150)) +
  scale_y_reverse() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey35", color = NA),
    plot.title = element_text(size = 40, hjust = 0, color = "grey95", family = f1, face = "bold"),
    plot.subtitle = element_text(family = f2b, margin = margin(7, 0, 40, 0), hjust = 0, size = 15, color = "grey80"),
    plot.margin = margin(20, 20, 20, 20)
  )
