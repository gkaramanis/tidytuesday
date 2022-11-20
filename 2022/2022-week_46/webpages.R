library(tidyverse)
library(ggforce)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')

speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv')

bytes_speed <- speed_index %>% 
  left_join(bytes_total, by = "timestamp") %>% 
  mutate(
    date.x = lubridate::as_date(date.x),
    client.x = if_else(client.x == "desktop", "🖥️  Desktop", "📱 Mobile")
    )

f1 <- "Outfit"
col1 <- "#435E55"
col2 <- "#5B84B1"

p <- ggplot(bytes_speed) +
  geom_ribbon(aes(x = date.x, ymin = p10.x, ymax = p90.x, group = client.x, color = client.x, fill = after_scale(colorspace::lighten(color, 0.8))), linewidth = 0.1) +
  geom_ribbon(aes(x = date.x, ymin = p25.x, ymax = p75.x, group = client.x, color = client.x, fill = after_scale(colorspace::lighten(color, 0.5))), linewidth = 0) +
  geom_line(aes(x = date.x, y = p50.x, group = client.x, color = client.x, linewidth = p50.y), lineend = "round") +
  scale_linewidth_continuous(range = c(0, 5)) +
  scale_color_manual(values = c(col1, col2)) +
  scale_fill_manual(values = c(col1, col2)) +
  facet_wrap(vars(client.x), ncol = 1) +
  labs(
    title = "Webpages get heavier and slower",
    subtitle = "Speed Index of top sites. The index shows how quickly the contents\n of a page are visibly populated, as measured with WebPageTest",
    caption = "Source: HTTP Archive · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    strip.text = element_text(size = 20, hjust = 0),
    plot.title = element_text(hjust = 0.5, size = 25),
    plot.subtitle = element_text(hjust = 0.5)
  )
  

annot <- tribble(
  ~x, ~y, ~l, ~h,
  "2020-12-15", 10.1, "90%\nof websites", 1,
  "2020-12-15", 6.5, "75%", 1,
  "2020-12-15", 4.4, "50%", 1,
  "2020-12-15", 2.8, "25%", 1,
  "2020-12-15", 1.6, "10%", 1,
  "2022-01-18", 3.9, "width of middle line\nshows page weight", 0
  )

leg <- bytes_speed %>% 
  filter(str_detect(client.x, "Desktop")) %>% 
  filter(between(date.x, as.Date("2021-01-01"), as.Date("2022-01-01"))) %>% 
  ggplot() +
  geom_ribbon(aes(x = date.x, ymin = p10.x, ymax = p90.x, group = client.x, color = client.x, fill = after_scale(colorspace::lighten(color, 0.8))), linewidth = 0.1) +
  geom_ribbon(aes(x = date.x, ymin = p25.x, ymax = p75.x, group = client.x, color = client.x, fill = after_scale(colorspace::lighten(color, 0.5))), linewidth = 0) +
  geom_line(aes(x = date.x, y = p50.x, group = client.x, color = client.x, linewidth = p50.y), lineend = "round") +
  geom_text(data = annot, aes(as.Date(x), y, label = l, hjust = h), family = f1, size = 3, lineheight = 0.8) +
  scale_color_manual(values = c("grey40", "grey80")) +
  scale_linewidth_continuous(range = c(0.5, 5)) +
  labs(title = "How to read") +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, hjust = 0.5, margin = margin(0, 0, 8, 0), color = "grey40")
  )

p +
  inset_element(leg, 0.65, 0.3, 0.9, 0.5) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
  )
    
