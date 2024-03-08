library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv') %>% 
  janitor::clean_names()

trashwheel_long <- trashwheel %>% 
  filter(!is.na(date)) %>% 
  mutate(date = parse_date_time(date, "m/d/y"))  %>% 
  pivot_longer(weight:sports_balls, names_to = "type") %>% 
  filter(type == "polystyrene")

f1 <- "Source Serif Pro"
f2 <- "Bricolage Grotesque"

ggplot(trashwheel_long, aes(date, value, color = name)) +
  geom_vline(xintercept = as_datetime("2018-04-17"), linewidth = 0.5, color = "#D2BFAB") +
  geom_vline(xintercept = as_datetime("2020-10-01"), linewidth = 0.5, color = "#D2BFAB") +
  geom_vline(xintercept = as_datetime("2019-10-19"), linewidth = 0.5, color = "#D2BFAB") +
  # As of April 17, 2018, there is an 18-month phase-in period to give industry time to comply
  annotate("text", x = as_datetime("2018-01-15"), y = 11100, label = str_wrap("In April 2017, Baltimore City passes a law banning polystyrene foam containers, with an 18-month phase-in period to give industry time to comply", 50), family = f1, hjust = 1, fontface = "bold", color = "#3D5269") +
  annotate("curve", xend = as_datetime("2018-04-01"), x = as_datetime("2017-11-01"), yend = 10000, y = 10300, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.4, color = "#D2BFAB") +
  # Baltimore City foam container ban takes effect October 19, 2019
  annotate("text", x = as_datetime("2020-02-01"), y = 11000, label = "Baltimore City foam container ban\ntakes effect in October 2019", family = f1, hjust = 0, fontface = "bold", color = "#3D5269") +
  annotate("curve", xend = as_datetime("2019-10-30"), x = as_datetime("2020-06-01"), yend = 10000, y = 10500, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.4, color = "#D2BFAB", curvature = -0.5) +
# Maryland's foam container ban takes effect October 1, 2020
  annotate("text", x = as_datetime("2021-02-01"), y = 8600, label = "Statewide ban takes effect\nin October 2020", family = f1, hjust = 0, fontface = "bold", color = "#3D5269") +
  annotate("curve", xend = as_datetime("2020-10-17"), x = as_datetime("2021-05-01"), yend = 7500, y = 8100, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.4, color = "#D2BFAB", curvature = -0.5) +
  # Lines annotations
  annotate("text", x = as_datetime("2016-12-01"), y = 7500, label = "Daily number of\npolystyrene items collected", hjust = 1, family = f1, lineheight = 0.9, color = "grey50") +
  annotate("curve", x = as_datetime("2017-01-01"), xend = as_datetime("2017-08-01"), y = 7300, yend = 7400, arrow = arrow(length = unit(0.1, "inch"), ), size = 0.4, color = "grey50", alpha = 0.5, curvature = 0.2) +
  annotate("text", x = as_datetime("2017-10-01"), y = 6500, label = "7-day average", hjust = 1, family = f1) +
  annotate("curve", x = as_datetime("2017-09-01"), xend = as_datetime("2018-03-01"), y = 6200, yend = 6300, arrow = arrow(length = unit(0.1, "inch")), size = 0.4, curvature = 0.2) +
  ## Trash wheel names
  geom_text(data = NULL, aes(x = as_datetime("2020-12-01"), y = 0, label = "Captain Trash Wheel"), hjust = 1, family = f2, stat = "unique", color = "tan2", size = 3.8, fontface = "bold") +
  geom_text(data = NULL, aes(x = as_datetime("2016-09-01"), y = 250, label = "Mister Trash Wheel"), hjust = 1, family = f2, stat = "unique", color = "grey15", size = 3.8, fontface = "bold") +
  geom_text(data = NULL, aes(x = as_datetime("2021-05-01"), y = 4000, label = "Professor Trash Wheel"), hjust = 0, family = f2, stat = "unique", color = "green4", size = 3.8, fontface = "bold") +
  geom_text(data = NULL, aes(x = as_datetime("2022-01-01"), y = 1200, label = "Gwynnda Trash Wheel"), hjust = 0, family = f2, stat = "unique", color = "magenta3", size = 3.8, fontface = "bold") +
  # original data
  geom_line(alpha = 0.2, linewidth = 0.3) +
  
  # 7-day moving average
  tidyquant::geom_ma(ma_fun = SMA, n = 7, linetype = 1) +
  # Scales and stuff
  scale_x_datetime(expand = expansion(mult = c(0.04, 0.02))) +
  scale_y_continuous(breaks = c(500, 1000, 1500, seq(0, 12000, 2000)), minor_breaks = seq(0, 12000, 1000), position = "right", labels = scales::number) +
  scale_color_manual(values = c("tan2", "magenta3", "grey15", "green4")) +
  labs(
    title = "Baltimore's Trash Wheels go on a diet thanks to foam container ban",
    y = "Polystyrene items collected",
    caption = "Source: Mr. Trash Wheel Baltimore Healthy Harbor · Graphic: Georgios Karamanis",
  ) +
  coord_cartesian(clip = "off") +
  theme_gray(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#F6F2EE", color = NA),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12, color = "#273B50"),
    plot.margin = margin(20, 20, 20, 30),
    plot.title = element_text(size = 20, face = "bold", color = "#273B50", margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(color = "#273B50"),
    plot.caption = element_text(color = "#273B50")
  )
