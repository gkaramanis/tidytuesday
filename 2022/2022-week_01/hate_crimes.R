library(tidyverse)
library(xlsx)
library(janitor)
library(ggpattern)
library(shadowtext)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# Inspiration
# https://www.axios.com/pandemic-helpline-loneliness-fear-suicide-254ce32f-7561-443f-b1dc-42919100a7dd.html

# Source
# https://bra.se/statistik/statistiska-undersokningar/hatbrottsstatistik/hatbrottsstatistik-2008-2018.html

tc_n <- read.xlsx(here::here("2022", "2022-week_01", "data", "Transfobiska motiv fr o m 2008.xls"), sheetIndex = 1, startRow = 4, endRow = 5) %>% 
  clean_names() %>% 
  select(-ar) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "year", values_to = "n")

tc_ci <- read.xlsx(here::here("2022", "2022-week_01", "data", "Transfobiska motiv fr o m 2008.xls"), sheetIndex = 1, startRow = 4, endRow = 7) %>% 
  clean_names() %>% 
  filter(row_number() == 3) %>% 
  select(-ar) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(everything(), names_to = "year", values_to = "ci")

tc <- left_join(tc_n, tc_ci) %>% 
  separate(ci, into = c("low_ci", "high_ci")) %>% 
  mutate(across(everything(), parse_number))

col = "mediumpurple2"
f1 = "Founders Grotesk"
f2 = "Input Mono"

ggplot(tc) +
  geom_col(aes(x = year, y = n), fill = col, width = 0.7) +
  geom_rect_pattern(aes(xmin = year - 0.35, xmax = year + 0.35, ymin = low_ci, ymax = high_ci), fill = NA, pattern_fill = colorspace::lighten(col, 0.3), pattern_color = NA, pattern_spacing = 0.0075, pattern_density = 0.4) +
  # "Dummy" geom for legend
  geom_tile_pattern(aes(2017, 0, pattern_fill = "whatever"), width = 0, height = 0, fill = NA, pattern_color = NA, pattern_spacing = 0.0075, pattern_density = 0.4) + 
  # Crimes
  geom_shadowtext(aes(x = year, y = n, label = n), nudge_y = -1.5, family = f1, fontface = "bold", size = 6, color = colorspace::lighten(col, 0.9), bg.colour = colorspace::darken(col, 0.9), bg.r = 0.07, hjust = 1) +
  # Years
  geom_text(aes(x = year, y = -2, label = year), hjust = 1, family = f2, size = 5) +
  # Annotations
  annotate("text", x = 2017, y = 0, label = "After 2016 hate crime statistics are published every second year", family = f1, hjust = 0, size = 5) +
  annotate("text", x = 2012, y = 57, label = str_wrap("Until 2011 the hate crime statistics were based on a census survey From 2012 onwards the figures are estimates based on a sample survey", 70), family = f1, hjust = 0, size = 3.5, lineheight = 0.9) +
  # Scales and theme
  scale_x_reverse() +
  scale_y_continuous(limits = c(-5, 100)) +
  scale_pattern_fill_manual(values = colorspace::lighten(col, 0.3), label = "95% confidence interval", name = NULL) +
  coord_flip() +
  labs(
    title = "Transphobic hate crimes in Sweden",
    subtitle = "Reported crimes that the police classified as hate crimes and where Brå* identified hate as the underlying motive",
    caption = "Source: *Swedish National Council for Crime Prevention (Brå, bra.se) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.125, 1.035),
    legend.text = element_text(size = 15),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 15, margin = margin(10, 0, 30, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )
