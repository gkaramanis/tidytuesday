library(scales)
library(tidyverse)
library(lubridate)

library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 10, height = 8, dpi = 320)


file_names <- list.files(here::here("temp-times", "data"), pattern = "*.csv", full.names = TRUE)

tmp_times <- do.call(rbind, lapply(file_names, read_csv)) 
  
tmp_hours <- tmp_times %>% 
  mutate(
    dec_time = hour(time) + minute(time) / 60 + second(time) / 3600,
    dn = if_else(between(dec_time, 6, 18), "day", "night"),
    hour = hour(time),
    hour_dn = ifelse(hour > 11, hour - 12, hour),
    month = month(day),
    date = day,
    day = day(date),
    wday = wday(date, label = TRUE, abbr = FALSE, week_start = 1)
  ) %>% 
  group_by(month, hour) %>% 
  add_count()

# tweets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/tidytuesday_tweets/data.csv")

# my_tweets <- tweets %>%
#   filter(year(created_at) == 2020 & month(created_at) > 5 & screen_name == "geokaramanis") %>% 
#   mutate(
#     tweet_dec_time = hour(created_at) + minute(created_at) / 60 + second(created_at) / 3600,
#     tweet_weekday = wday(created_at, label = TRUE, abbr = FALSE, week_start = 1)
#          ) %>% 
#   select(created_at, tweet_dec_time, tweet_weekday)

ggplot(tmp_hours) +
  geom_point(aes(x = date, y = dec_time), size = 0.25, alpha = 0.5) +
  # geom_segment(data = subset(tmp_hours, wday == "Saturday" | wday == "Sunday"), aes(x = date, xend = date, y = 0, yend = 24)) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%d %b %Y") +
  scale_y_reverse(breaks = 0:23, labels = paste0(str_pad(0:23, "left", pad = 0, width = 2), ":00")) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank()
  )
  


f1 <- "Outfit"

ggplot(tmp_hours, aes(x = hour_dn, y = month, fill = n)) +
  geom_tile(stat = "unique", color = NA) +
  # hours
  geom_text(aes(y = 18, label = hour), stat = "unique", family = f1, size = 7, color = "#b57e65") +
  # arrows
  annotate("segment", x = 5.5, y = 0, xend = 5.5, yend = 13,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.75, color = "grey30") +
  annotate("segment", x = 6.3, y = 13.6, xend = 7.1, yend = 13.6,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.75, color = "grey30") +
  # arrows "labels"
  annotate("text", x = 5.25, y = 6, label = "month", angle = -73, family = f1, size = 4) +
  annotate("text", x = 6, y = 13.6, label = "hours", angle = -0, family = f1, size = 4) +
  # day-night
  scale_x_continuous(breaks = 0:11) +
  scale_y_continuous(limits = c(-4, 19)) +
  # scale_fill_stepsn(colors = pal) +
  coord_polar(start = -pi/12, clip = "off") +
  facet_wrap(vars(dn)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height = unit(0.75, "line"),
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20, family = f1, color = "#9865b5"),
    plot.subtitle = element_text(hjust = 0.5, family = f1, color = "#b57e65", margin = margin(10, 0, 0, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), family = f1, color = "#b57e65"),
    plot.margin = margin(20, 10, 20, 10)
  )
