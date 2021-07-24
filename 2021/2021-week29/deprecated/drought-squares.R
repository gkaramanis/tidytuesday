library(tidyverse)
library(lubridate)
library(camcorder)
library(patchwork)

gg_record(dir = "temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

d234_lvl <- drought %>% 
  mutate(
    drought_lvl = fct_relevel(drought_lvl, c("None", "D0", "D1", "D2", "D3", "D4")),
    year = year(valid_start),
    week = week(valid_start),
    side = sqrt(area_pct / 100)
    ) %>% 
  filter(state_abb %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")) %>% 
  select(state_abb, drought_lvl, area_pct, side, year, week) %>% 
  filter(drought_lvl %in% c("D2", "D3", "D4")) 
  pivot_wider(names_from = drought_lvl, values_from = c(area_pct, side))

d234_lvl_p1 <- d234_lvl %>% 
  filter(between(year, 2016, 2018))

d234_lvl_p2 <- d234_lvl %>% 
  filter(between(year, 2019, 2021))

f1 = "Founders Grotesk Condensed"
f1b = "Founders Grotesk Condensed Bold"

p1 <- ggplot(d234_lvl_p1) +
  geom_tile(aes(x = week, y = as.numeric(factor(state_abb)) * 3, width = side, height = side, fill = drought_lvl)) +
  # scale_x_continuous(limits = c(1, 53)) +
  scale_fill_manual(values = c("orange", "red", "darkred")) +
  coord_fixed() +
  facet_wrap(vars(year), ncol = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

p2 <- ggplot(d234_lvl_p2) +
  geom_tile(aes(x = week, y = as.numeric(factor(state_abb)) * 3, width = side, height = side, fill = drought_lvl)) +
  # scale_x_continuous(limits = c(1, 53)) +
  scale_fill_manual(values = c("orange", "red", "darkred")) +
  coord_fixed() +
  facet_wrap(vars(year), ncol = 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )

p1 | p2

d234_lvl %>% 
  arrange(year) %>% 
  filter(drought_lvl == "D4") %>% 
  ggplot() +
  geom_tile(aes(x = week, y = as.numeric(factor(state_abb)) * 3 + (year -2000)/9, width = side, height = 0.1, fill = if_else(year == 2021, "darkred", "transparent"), color = if_else(year == 2021, "darkred", "grey20")), size = 0.1) +
  scale_fill_identity() +
  scale_color_identity() +
  # scale_fill_manual(values = c("orange", "red", "darkred")) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )

# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" video.mp4