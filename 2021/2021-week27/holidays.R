library(tidyverse)
library(camcorder)
library(lubridate)
library(geofacet)
library(countrycode)
library(ggtext)

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

holiday_months <- holidays %>% 
  filter(!is.na(date_parsed)) %>% 
  select(country, date_parsed, day) %>% 
  mutate(
    month_num = month(ymd(date_parsed)),
    month_label = month(month_num, label = TRUE),
    country_code = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_code = if_else(country == "Micronesia", "FSM", country_code)
  ) %>% 
  group_by(month_num, day) %>% 
  mutate(
    n = n(),
    all_codes = paste(country_code, collapse = "\n")
  ) %>% 
  ungroup() %>% 
  complete(month_num, day, fill = list(n = 2)) %>% 
  mutate(
    month_label = month(month_num, label = TRUE),
    month_label = fct_rev(month_label)
    ) %>% 
  filter(!(month_label %in% c("Apr", "Jun", "Sep", "Nov") & day == 31)) %>% 
  filter(!(month_label == "Feb" & day > 29))

country_list <- holiday_months %>% 
  distinct(country_code, country) %>% 
  filter(!is.na(country)) %>% 
  arrange(country_code) %>% 
  group_by(first_char = substring(country_code, 1, 1)) %>% 
  mutate(
    country = case_when(
      country_code == "COD" ~ "Democratic Rep. of the Congo",
      country_code == "COG" ~ "Republic of the Congo",
      country_code == "VCT" ~ "St. Vincent & the Grenadines",
      country_code == "BHS" ~ "The Bahamas",
      country_code == "GMB" ~ "The Gambia",
      country_code == "NLD" ~ "The Netherlands",
      TRUE ~ country
    ),
    bold = if_else(row_number() == 1, TRUE, FALSE),
    country = if_else(bold, paste0("**", country, "**"), country),
    country_code = if_else(bold, paste0("**", country_code, "**"), country_code)
    ) %>%
  ungroup() %>% 
  group_by(x = rep(c(0, 1), each = n()/2)) %>% 
  mutate(y = seq(12, 0.75, length.out = n())) %>% 
  ungroup()

f1 = "KyivType Titling"
f2 = "KyivType Sans"

gg_record(dir = "temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

ggplot(holiday_months) +
  # days
  geom_text(aes(day, month_label, label = ifelse(!is.na(country), day, "·"), size = n), stat = "unique", family = f1) +
  # country codes
  geom_text(aes(day, month_label, label = ifelse(!is.na(country), all_codes, "")), size = 2, nudge_y = -0.2, stat = "unique", family = f2, vjust = 1, lineheight = 0.9, color = "#936900") +
  # country list
  geom_richtext(data = subset(country_list, x == 0), aes(x = 37, y = y, label = paste0(country, " ", country_code)), hjust = 1, size = 1.75, family = f2, fill = NA, label.color = NA) +
  geom_richtext(data = subset(country_list, x == 1), aes(x = 37, y = y, label = paste0(country_code, " ", country)), hjust = 0, size = 1.75, family = f2, fill = NA, label.color = NA) +
  geom_text(data = country_list, aes(x = 37, y = y, label = "·"), family = f2, color = "#936900", stat = "unique", nudge_y = 0.0225) +
  scale_size_continuous(range = c(3.5, 7)) +
  scale_x_continuous(limits = c(0, 42)) +
  scale_y_discrete(labels = toupper) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = toupper("ᨓᨐᨏ National Independence Days ᨏᨐᨓ"),
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = "none",
    axis.text.y = element_text(family = f1),
    plot.margin = margin(20, 20, 10, 20),
    plot.title = element_text(family = f2, size = 20, hjust = 0.5, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(family = f2, size = 8, hjust = 0.5, margin = margin(20, 0, 0, 0), color = "#936900")
  )

# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" video.mp4
