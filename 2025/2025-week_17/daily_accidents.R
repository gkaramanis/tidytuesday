library(tidyverse)
library(slider)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 7, units = "in", dpi = 320)

daily_accidents <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents.csv')

acc_comp <- daily_accidents %>% 
  arrange(date) %>%
  mutate(
    fatalities_rolling_mean = slide_dbl(
      fatalities_count,
      mean,
      .before = 30,
      .after = 30,
      .complete = TRUE
    )
  ) %>% 
  rowwise() %>% 
  mutate(fatalities_compare = fatalities_count / fatalities_rolling_mean)

acc_holidays <- acc_comp %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>%
  mutate(
    date_is_first_dst = date == min(date[dst(as.POSIXct(date, tz = "America/New_York"))] - 1),
    date_is_last_dst = date == max(date[dst(as.POSIXct(date, tz = "America/New_York"))]),
  ) %>%
  ungroup() %>% 
  mutate(
    date_is_420 = if_else(str_detect(date, "-04-20"), TRUE, FALSE),
    date_is_july_4 = if_else(str_detect(date, "-07-04"), TRUE, FALSE),
    date_is_new_year = if_else(str_detect(date, "-01-01"), TRUE, FALSE),
    date_is_xmas = if_else(str_detect(date, "-12-25"), TRUE, FALSE)
  ) %>% 
  pivot_longer(cols = starts_with("date_is_"),
               names_to = "holiday",
               values_to = "is_holiday")
  # filter(is_holiday == TRUE)

acc <- acc_holidays %>% 
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 1),
    yearday = yday(date)
  )

acc_points <- acc %>% 
  select(-holiday, -is_holiday) %>% 
  distinct()

# not used
acc_hol <- acc %>% 
  filter(is_holiday)  

f1 <- "Fixel Display"
f2 <- "Borel"

ggplot(acc_points, aes(x = fatalities_count, y = fatalities_compare)) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = median(acc_points$fatalities_count, na.rm = TRUE), linetype = "dashed") +
  ggpointdensity::geom_pointdensity() +
  scale_y_log10() +
  MetBrewer::scale_color_met_c("Hokusai1", direction = -1) +
  facet_wrap(vars(weekday), nrow = 2) +
  labs(
    title = "Fatal accidents peak on weekends",
    subtitle = str_wrap("Each point represents a day between 2010 and 2023. Fridays, Saturdays, and Sundays show higher fatality counts (x-axis) and are also generally higher compared to their respective 60-day rolling averages (y-axis). The horizontal line marks a ratio of 1 (daily fatalities equal the rolling average), while the dashed vertical line indicates the overall median daily fatalities.", 125),
    caption = "Source: Fatality Analysis Reporting System (NHTSA) · Graphic: Georgios Karamanis",
    x = "Fatalities per day",
    y = "Fatalities vs 60-day average (log scale)"
  ) +
  coord_cartesian(clip = "off") +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#d1e3f3", color = "black"),
    strip.text = element_text(face = "bold", colour = "grey10", size = 11),
    axis.text = element_text(family = f2),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 11, lineheight = 1.1, margin = margin(0, 0, 20, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
