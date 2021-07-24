library(tidyverse)
library(lubridate)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 6, height = 12, units = "in", dpi = 320)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

west_grid <- us_state_grid1 %>% 
  filter(code %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

d4_lvl <- drought %>% 
  mutate(
    drought_lvl = fct_relevel(drought_lvl, c("None", "D0", "D1", "D2", "D3", "D4")),
    year = year(valid_start),
    week = week(valid_start),
    side = sqrt(area_pct / 100)
    ) %>% 
  filter(drought_lvl %in% c("D4")) %>%
  filter(week == 28) %>% 
  filter(state_abb %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")) %>% 
  left_join(west_grid, by = c("state_abb" = "code"))

d4_lvl_plot <- d4_lvl %>% 
  mutate(area_pct = pmax(area_pct, 0.5)) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(0,
               area_pct/100 * sqrt(3)/2,
               0,
               area_pct/100 * -sqrt(3)/2)),
    y = list(c(-area_pct/100 * sqrt(0.5)/2,
               0,
               area_pct/100 * sqrt(0.5)/2,
               0))
  ) %>% 
  unnest(c(x, y))


ggplot(d4_lvl_plot) +
  geom_text(aes(x = col * 1.5 + area_pct/100 * sqrt(3)/2  + 0.1,
                y = -row * 1.5 + year/50,
                label = if_else(area_pct > 1 | (area_pct < 1 & year %% 2 == 0) | year == 2021, year, NULL)),
            stat = "unique", size = 1, family = "Input Mono Compressed") +
  geom_segment(aes(x = col * 1.5 + area_pct/100 * sqrt(3)/2  + 0.05,
               y = -row * 1.5 + year/50,
               xend = col * 1.5 + + area_pct/100 * sqrt(3)/2 + 0.02,
               yend = -row * 1.5 + year/50,
               size = if_else(area_pct > 1 | (area_pct < 1 & year %% 2 == 0)  | year == 2021, 0.07, 0)),
               color = "grey60"
               ) +
  geom_polygon(aes(x = col * 1.5 + x,
                   y = -row * 1.5 + y + year/50,
                   group = interaction(name, year, week),
                   fill = if_else(year == 2021, "darkred", "grey50"),
                   color = if_else(year == 2021, "red", "grey20")),
               alpha = 0.7, size = 0.1) +
  geom_text(aes(x = col * 1.5,
                y = -row * 1.5 + 2045/50,
                label = name),
            stat = "unique", family = "Fira Sans Condensed") +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  coord_fixed() +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" video.mp4