library(tidyverse)
library(ggforce)
library(ggnewscale)
library(ggflags)
library(countrycode)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

wheels_chart <- wheels %>% 
  select(name, country, diameter, status, type) %>% 
  mutate(
    # height_m = height / 3.281,
    diameter_m = round(diameter / 3.281),
    wheel_label = if_else(!is.na(diameter_m), paste0(name, " · ", diameter_m, "\u00A0m"), name), # non-breaking space before m
    x = rep(1:9, length.out = n()),
    y = rep(9:1, each = 9, length.out = n()),
    country = case_when(
      str_detect(country, "Dubai") ~ "United Arab Emirates",
      str_detect(country, "Philli") ~ "Philippines",
      str_detect(country, "Tailand") ~ "Thailand",
      TRUE ~ country
    ),
    code = countrycode(country, "country.name", "iso2c"),
    status = case_when(
      str_detect(status, "Design") ~ "In development",
      str_detect(status, "Under") ~ "In development",
      str_detect(status, "SBNO") ~ "In development",
      str_detect(status, "Delayed") ~ "In development",
      str_detect(status, "Planned") ~ "In development",
      str_detect(status, "Moved") ~ "Relocated",
      is.na(status) ~ "Unknown",
      TRUE ~ status
    ),
    type = case_when(
      is.na(type) ~ "Unknown",
      TRUE ~ type
    )
  ) 
  
f1 <- "Outfit"
f2 <- "October Condensed Devanagari"

bg <- "#fcfdfc"
tiles_pal <- c("#e98092", "#86cfcd", "#7db658", "#f8c129", "#dcdce5")
wheels_pal <- c("#66cdaa", "#ff8c00", "#bbf10a", "#1e90ff", "#eb1461", "#543ede", "#dcdce5")

p <- ggplot(wheels_chart) +
  # Tile = status
  geom_tile(aes(x, y, fill = status, width = 0.98, height = 0.98)) +
  scale_fill_manual(values = tiles_pal, name = "Status") +
  new_scale_fill() +
  # Flag
  geom_flag(aes(x - 0.37, y + 0.37, country = tolower(code)), size = 4) +
  # Circle = diameter, type
  geom_circle(data = wheels_chart %>% filter(!is.na(diameter_m)), aes(x0 = x, y0 = y, r = diameter_m/600, fill = type, color = after_scale(colorspace::darken(fill, 0.2))), size = 0.3) +
  # Unknown diameter
  geom_regon(data = wheels_chart %>% filter(is.na(diameter_m)), aes(x0 = x, y0 = y, r = 0.15, sides = 5, angle = 0, fill = type, color = after_scale(colorspace::darken(fill, 0.2)))) +
  # Labels - Name and diameter
  geom_text(aes(x, y - 0.45, label = str_wrap(wheel_label, 20)), size = 2, vjust = 0, lineheight = 0.9, family = f2) +
  # Title, subtitle, caption
  annotate("text", -1.65, 9.45, label = "Ferris\nWheels", hjust = 0, vjust = 1, size = 11, family = f1, fontface = "bold", lineheight = 0.9, color = "purple4") +
  annotate("text", -1.65, 8.3, label = str_wrap("Country, diameter, status and type of the 73 Ferris wheels in the ferriswheels package. 'Planned', 'design/finance', 'under construction', 'delayed' and 'SBNO' statuses were incorporated into 'In development'. 'Moved' status was included in 'Relocated'.", 23), hjust = 0, vjust = 1, size = 4, family = f1, lineheight = 1, color = "grey20") +
  annotate("text", -1.6, 0.55, label = "Source: ferriswheels R package\nGraphic: Georgios Karamanis", hjust = 0, vjust = 0, size = 2.8, family = f1, color = "grey30") +
  # Extra legend
  geom_regon(data = NULL, aes(x0 = -1.5, y0 = 1.3, r = 0.09, sides = 5, angle = 0), fill = NA, color = "grey40", size = 0.4) +
  annotate("text", -1.33, 1.35, label = "Unknown diameter", hjust = 0, vjust = 1, size = 3, family = f1, color = "grey20") +
  # Scales, theme and stuff
  scale_fill_manual(values = wheels_pal, name = "Type") +
  coord_fixed(clip = "off", expand = FALSE) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.06, 0.3),
    legend.key.size = unit(0.8, "lines"),
    legend.margin = margin(0, 0, 15, 0),
    legend.title = element_text(face = "bold", color = "grey20"),
    legend.text = element_text(color = "grey20"),
    plot.background = element_rect(fill = bg, color = NA),
    plot.margin = margin(10, 7, 10, 7)
  )
    

# Flags legend
flags_legend <- wheels_chart %>% 
  distinct(country, code) %>% 
  arrange(country) %>% 
  mutate(id = row_number()) %>% 
  ggplot() +
  geom_flag(aes(x = (id - 1) %% 13, y = (id - 1) %/% 13, country = tolower(code)), size = 4) +
  geom_text(aes(x = (id - 1) %% 13, y = (id - 1) %/% 13, label = str_wrap(country, 15)), nudge_y = -0.3, family = f1, vjust = 1, lineheight = 0.9, size = 2, color = "grey40") +
  scale_y_reverse() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void()

# Combine plots
p +
  inset_element(flags_legend, 0.32, 0.01, 0.97, 0.08) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, color = NA)
    )
  )

