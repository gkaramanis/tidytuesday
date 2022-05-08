library(tidyverse)
library(camcorder)
library(ggtext)

# code exporter

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 9, units = "in", dpi = 320)

wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')

wind_m <- wind %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% 
  summarise(wind_m = median(wind_mwh))

solar_m <- solar %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% 
  summarise(solar_m = median(solar_mwh))

windsolar <- wind_m %>% 
  left_join(solar_m) %>% 
  pivot_longer(2:3)

seasons <- data.frame(
  x = c(4, 7, 10, 12, 1.5),
  y = 45,
  height = 90,
  width = c(3, 3, 3, 1, 2),
  season = c("Spring", "Summer", "Fall", "Winter", "Winter")
)

f1 <- "Plus Jakarta Sans"

ggplot(windsolar) +
  geom_tile(data = seasons, aes(x, y, height = height, width = width, fill = season)) +
  # geom_hline(yintercept = seq(0, 80, 10), size = 0.1, alpha = 0.2) +
  annotate("segment", x = -Inf, xend = Inf, y = seq(0, 80, 10), yend = seq(0, 80, 10), size = rep(c(0.25, 0.1), length.out = 9), alpha = 0.2) +
  annotate("label", x = 0.5, y = seq(20, 80, 20), label = seq(20, 80, 20), family = f1, size = 3, fill = "#FEF8FA", color = "grey40", label.padding = unit(0.1, "lines"), label.size = 0) +
  geom_col(aes(x = month, y = value, fill = name), position = "dodge", width = 0.6) +
  scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12, labels = month.abb) +
  scale_y_continuous(limits = c(-30, 90)) +
  scale_fill_manual(values = c("#F9CC87", "#FC766AFF", "#D6F1C6", "#C9F1FD", "#5B84B1FF", "#FEF8FA")) + # Fall, solar, Spring, Summer, wind, Winter
  coord_polar() +
  labs(
    title = "<span style='color:#FC766AFF'>Solar</span> and <span style='color:#5B84B1FF'>wind</span>",
    subtitle = "Median projected prices in 2020 $/MWh by month<br>For <span style='color:#FC766AFF'>Feb 2009 - Jun 2021</span> and <span style='color:#5B84B1FF'>Jan 2009 - Jun 2020</span>",
    caption = "Source:  Berkeley Lab · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey94", color = NA),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 14),
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 25, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_markdown(hjust = 0.5, size = 14, lineheight = 1.4),
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0))
  )
  
