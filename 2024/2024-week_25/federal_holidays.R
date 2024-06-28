library(tidyverse)
library(ggcalendar)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# federal_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/federal_holidays.csv')
# proposed_federal_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/proposed_federal_holidays.csv')

## Get public holidays from Nager.Date and write a CSV
# 
# holidays <- function(year) {
#   url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/US")
# 
#   lapply(url, jsonlite::fromJSON) %>% 
#     bind_rows()
# }
# 
# holidays_historical <- holidays(1975:2024)
# 
# write_csv(holidays_historical, here::here("2024/2024-week_25/data/holidays_historical.csv"))

holidays_historical <- read_csv(here::here("2024/2024-week_25/data/holidays_historical.csv"))

holidays_dec <- holidays_historical %>% 
  mutate(
    name = case_when(
      str_detect(name, "Indigenous|Columbus") ~ "Indigenous Peoples' Day\nColumbus Day",
      TRUE ~ name
    )
  ) %>% 
  distinct(name, date) %>% 
  mutate(
    date = as.Date(date),
    year = year(date),
    dd = decimal_date(date) - year,
    days_since_last_holiday = date - lag(date),
    longest_gap = if_else(days_since_last_holiday == max(days_since_last_holiday, na.rm = TRUE), TRUE, FALSE),
    longest_gap_dates = case_when(
      !longest_gap & lead(longest_gap) ~ TRUE,
      longest_gap ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  group_by(name) %>% 
  mutate(
    ddm = mean(dd),
    y = min(year),
    ddm = if_else(str_detect(name, "New Year"), 0, ddm)
    ) %>% 
  ungroup()
  
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(holidays_dec) +
  geom_path(data = . %>% filter(longest_gap_dates), aes(year, dd, group = year), color = "#b6ada9") +
  geom_point(data = . %>% filter(longest_gap_dates), aes(year, dd), size = 3, shape = 21, fill = "grey99", color = "#b6ada9") +
  geom_point(aes(year, dd, color = name)) +
  geom_text(aes(y - 0.8, ddm, label = name), stat = "unique", hjust = 1, family = f1b, lineheight = 0.9, size = 3.5) +
  scale_x_continuous(breaks = c(seq(min(holidays_dec$year), max(holidays_dec$year), 5), max(holidays_dec$year))) +
  scale_y_reverse(position = "right", breaks = unique(holidays_dec$ddm), labels = function(x) str_remove(format(date_decimal(x), "%d %b"), "^0")) +
  MetBrewer::scale_color_met_d("Renoir") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Dry spells: When federal holidays take a break",
    subtitle = str_wrap("The chart shows the longest intervals between consecutive US federal holidays from 1975 to 2024. The longest gaps, each spanning 66 days, occur between Independence Day and Labor Day, and between Good Friday and Memorial Day.", 120),
    caption = "Source: Nager.Date · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_line(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(family = f2, face = "bold", size = 16),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 90)
  )