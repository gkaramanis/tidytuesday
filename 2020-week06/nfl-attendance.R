library(tidyverse)
library(here)
library(gghighlight)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

year_lines <- data.frame(year = seq(2000, 2019, 2)) %>%
  mutate(year_label = case_when(
  year > 2000 ~ substring(as.character(year), 3, 4),
  TRUE ~ as.character(year)))
 
# attendance %>% 
#  distinct(team_name, year, total) %>% 
#  group_by(team_name) %>% 
#  mutate(
#    next_year = lead(year, 1),
#    next_total = lead(total, 1)
#  ) %>% 
#  ggplot() +
#  geom_segment(aes(x = year, y = total,
#                   xend = next_year, yend = next_total,
#                   color = team_name), size = 1.2, alpha = 0.9)

ggplot(attendance) +
  geom_line(aes(year, total, group = team_name, color = team_name), size = 1.2, alpha = 0.9) +
  gghighlight(team_name == "Cowboys" | team_name == "Chargers", use_direct_label = FALSE) +
  # year lines
  geom_text(data = year_lines, aes(year, 250000, label = year_label), family = "JetBrains Mono Medium", colour = "white", angle = 0, size = 8) +
  geom_text(data = year_lines, aes(year, 1750000, label = year_label), family = "JetBrains Mono", colour = "white", angle = 180, size = 8) +
  # annotations
  annotate("label", 2007.5, 1420000, label = "Move to AT&T Stadium", family = "JetBrains Mono Bold", color = "white", fill = "#003594", label.size = 0) +
  annotate("label", 2015.5, 680000, label = "Move to Los Angeles", family = "JetBrains Mono Bold", color = "white", fill = "#F0AE00", label.size = 0) +
  # scales
  scale_y_continuous(limits = c(0, 2000000), breaks = c(0, 500000, 1000000, 1500000), labels = c("0", "50k", "100k", "150k"), position = "right") +
  scale_x_continuous(breaks = seq(2000, 2019, 2)) +
  scale_colour_manual(values = c("#F0AE00", "#003594"), labels = c("LA Chargers", "Dallas Cowboys"), guide = guide_legend(label.position = "left", reverse = TRUE)) +
  
  labs(
    title = "NFL Stadium Attendance 2000-2019",
    subtitle = str_wrap("The Dallas Cowboys have the highest total home attendance since 2009 when they moved to a bigger stadium, while the Chargers dropped to the last place when the team moved from San Diego to Los Angeles in 2017.", 75),
    caption = "Source: Pro Football Reference | Graphic: Georgios Karamanis"
  ) +
  
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(colour = "white", family = "IBM Plex Sans Medium", size = 12),
    legend.position = c(-0.05, 0.52),
    plot.background = element_rect(fill = "#6C8E68", colour = NA),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.length.y = unit(3, "mm"),
    axis.ticks.y = element_line(colour = "white"),
    axis.text.y = element_text(colour = "white", size = 10),
    plot.title = element_text(size = 20, hjust = 0.5, colour = "white", margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "white", margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, colour = "white", margin = margin(20, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 110)
  ) +
  ggsave(here::here("2020-week06", "plots", "nfl-attendance.png"), dpi = 320,
         height = 8, width = 12)
