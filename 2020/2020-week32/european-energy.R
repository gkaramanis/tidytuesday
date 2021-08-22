library(tidyverse)
library(geofacet)
library(futurevisions)
library(colorspace)
library(cowplot)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv') %>% 
  pivot_longer(cols = "2016":"2018", names_to = "year", values_to = "value") %>% 
  mutate(
    country_name = if_else(country == "UK", "United Kingdom", country_name),
    year = as.numeric(year)
  ) %>% 
  filter(year == 2018)

geoids <- geofacet::europe_countries_grid1 %>% 
  add_row(row = 4, col = 1, code = "UK", name = "United Kingdom") %>% 
  add_row(row = 6, col = 10, code = "GE", name = "Georgia") %>% 
  mutate(
    row = -row * 10, col = col * 10,
    code = if_else(code == "GR", "EL", code),
    name = case_when(
      grepl("Macedonia", name) ~ "North Macedonia",
      grepl("Bosnia", name) ~ "Bosnia & Herzegovina",
      TRUE ~ name
    )) %>% 
  set_names("latitude", "longitude", "country", "country_name") 


power_lines <- energy_types %>%
  mutate(type = recode(type, "Conventional thermal" = "Fossil", "Wind" = "Renewable", "Hydro" = "Renewable", "Pumped hydro power" = "Renewable", "Solar"  = "Renewable", "Geothermal"  = "Renewable")) %>% 
  group_by(country, type) %>% 
  mutate(value = sum(value)) %>% 
  ungroup() %>% 
  distinct(country, type, value, .keep_all = TRUE) %>% 
  left_join(geoids) %>%
  group_by(type) %>%
  mutate(g = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(
    total = sum(value),
    pct = value / total * 100) %>% 
  ungroup() %>% 
  mutate(
    n = recode(g, -0.8, 0.4, -0.4, 0.8), # where the power lines start
    a = if_else(n > 0, 30, 210), # angle
    x = longitude + n * cos(a * pi / 180), # x, y of the start points (found an easier way to calculate but didn't change)
    y = 3 + latitude + n * sin(a * pi / 180),
    xend = x + pct * cos(-a * pi / 180) / 15, # x, y of the end points
    yend = y + pct * sin(-a * pi / 180) / 15
  )

# towers
towers <- power_lines %>% 
  group_by(country) %>% 
  mutate(
    tx = longitude + 0.8,
    txend = longitude,
    ty = 3 + latitude + 0.5,
    tyend = 3 + latitude
  ) %>% 
  distinct(country, country_name, longitude, latitude, tx, ty, txend, tyend)

# palette
# pal <- futurevisions("pegasi")[c(3, 4, 5, 6)]

# main energy source
main <- power_lines %>% 
  group_by(country) %>% 
  slice_max(value) %>% 
  # select(country, country_name, type) %>% 
  ungroup() %>% 
  mutate(n = row_number())

# total by country, make a tile
totals <- power_lines %>% 
  rowwise() %>% 
  mutate(
    r = total / 30000 / 2,
    x = list(c(longitude, longitude + r, longitude, longitude - r)), # easier way to make isometric tile
    y = list(c(latitude - r/2, latitude, latitude + r/2, latitude)),
  ) %>% 
  unnest(c(x, y))

# country codes and names
cc <- main %>% unite("country", country, country_name, sep = " ") %>% pull(country) %>% paste(collapse = " • ")

# main plot
map <- ggplot(power_lines) +
  # totals tiles
  geom_polygon(data = totals, aes(x, y, group = country), fill = "#4A5F71", alpha = 0.7) +
  # tower
  geom_segment(data = towers, aes(x = tx, y = ty, xend = txend, yend = tyend), size = 0.8, lineend = "round", colour = "grey20") +
  geom_segment(data = towers, aes(x = tx + (txend -tx) / 2, y = ty + (tyend - ty) / 2, xend = tx + (txend -tx) / 2, yend = latitude), size = 0.8, lineend = "round", colour = "grey20") +
  # country names
  geom_text(data = main, aes(x = longitude, y = latitude, label = country), size = 4, hjust = 1, vjust = 1, nudge_x = -0.3, nudge_y = 0.5, family = "Proxima Nova Bold", lineheight = 0.9, colour = "grey85") +
  geom_text(data = main, aes(x = longitude, y = latitude, label = type), size = 3, hjust = 1, vjust = 1, nudge_x = -0.3, nudge_y = -1.1, family = "Produkt Light", colour = "grey85") +
  # lines
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.4) +
  # annotation
  annotate("text", 1, -90, label = str_wrap(cc, 170), size = 3.2, vjust = 1, hjust = 0, family = "Proxima Nova Medium", colour = darken("#4A5F71", 0.1)) +
  # scales, theme, etc
  coord_cartesian(clip = "off") +
  # scale_color_manual(values = pal) +
  labs(
    title = "Net electricity generation in 2018",
    subtitle = "The power lines show each type of energy (fossil, nuclear, renewable & other) as a percentage\nof the total energy produced in each country of the EU, current and potential candidate countries.",
    caption = "Source: Eurostat | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 30, 30),
    plot.background = element_rect(fill = lighten("#4A5F71", 0.3), colour = NA),
    plot.title = element_text(size = 28, colour = "grey98", family = "Proxima Nova Bold"),
    plot.subtitle = element_text(size = 18, colour = "grey98", family = "Proxima Nova", margin = margin(10, 0, 80, 0)),
    plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(20, 0, 0, 0), family = "Proxima Nova Medium", colour = darken("#4A5F71", 0.8))
  )

# legend inset
inset <- ggplot(subset(power_lines, country == "DE")) +
  # totals tiles
  geom_polygon(data = subset(totals, country == "DE"), aes(x, y, group = country), fill = "#4A5F71", alpha = 0.7) +
  # tower
  geom_segment(data = subset(towers, country == "DE"), aes(x = tx, y = ty, xend = txend, yend = tyend), size = 1, lineend = "round", colour = "grey20") +
  geom_segment(data = subset(towers, country == "DE"), aes(x = tx + (txend -tx) / 2, y = ty + (tyend - ty) / 2, xend = tx + (txend -tx) / 2, yend = latitude), size = 0.8, lineend = "round", colour = "grey20") +
  # country name and main type
  geom_text(data = subset(main, country == "DE"), aes(x = longitude, y = latitude, label = "Country"), size = 5, hjust = 1, vjust = 1, nudge_x = -0.3, nudge_y = 0.5, family = "Proxima Nova Bold", lineheight = 0.9, colour = "grey85") +
  geom_text(data = subset(main, country == "DE"), aes(x = longitude, y = latitude, label = "Main type"), size = 4, hjust = 1, vjust = 1, nudge_x = -0.3, nudge_y = -0.6, family = "Produkt Light", colour = "grey85", alpha = 1) +
  # lines
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.6) +
  # annotations
  annotate("text", 44.5, -34.5, label = "Fossil", size = 4, vjust = 1, hjust = 0, family = "Proxima Nova Bold") +
  annotate("text", 44.5, -35.5, label = "Other", size = 4, vjust = 1, hjust = 0, family = "Proxima Nova Bold") +
  annotate("text", 54, -37.5, label = "Reneweable", size = 4, vjust = 1, hjust = 0, family = "Proxima Nova Bold") +
  annotate("text", 54, -38.5, label = "Nuclear", size = 4, vjust = 1, hjust = 0, family = "Proxima Nova Bold") +
  annotate("text", 55.1, -43, label = "Total energy produced", size = 4.5, angle = 27, colour = "#4A5F71", family = "Proxima Nova Bold") +
  # thems, scales, etc
  coord_fixed(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(30, 5, 50, 5),
    plot.background = element_rect(fill = NA, colour = NA)
  ) 

ggdraw(map) +
  draw_plot(inset, x = -0.3, y = 0.25, scale = 0.3) 

ggsave(here::here("temp", paste0("european-energy-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 12)
