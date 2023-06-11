library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

eu_grid <- europe_countries_grid1 %>% 
  mutate(name = case_when(
    str_detect(name, "Moldova") ~ "Moldova",
    str_detect(name, "Macedonia") ~ "N. Macedonia",
    str_detect(name, "Bosnia") ~ "Bosnia & H.",
    str_detect(name, "Russian") ~ "Russia",
    str_detect(name, "Turkey") ~ "Türkiye",
    TRUE ~ name
  ))

europe_renewables <- owid_energy %>% 
  mutate(country = case_when(
    # country == "United Kingdom" ~ "UK",
    country == "North Macedonia" ~ "N. Macedonia",
    country == "Bosnia and Herzegovina" ~ "Bosnia & H.",
    country == "Turkey" ~ "Türkiye",
    TRUE ~ country
  )) %>% 
  filter(country %in% eu_grid$name)

f1 <- "Outfit"

europe_renewables %>% 
  select(country, year, renewables_share_elec, fossil_share_elec) %>% 
  ggplot() +
  geom_area(aes(x = year, y = renewables_share_elec, group = country), linewidth = 0.1, fill = "#0474BA") +
  geom_line(aes(x = year, y = fossil_share_elec, group = country), color = "orange2") +
  scale_x_continuous(limits = c(1985, 2022), breaks = c(1985, 2022)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100), labels = scales::percent_format(scale = 1)) +
  facet_geo(vars(country), grid = eu_grid) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Share of electricity generation from <span style='color:#0474BA'>**renewable**</span> and <span style='color:orange2'>**fossil fuels**</span>",
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(hjust = c(0, 1), color = "grey60"),
    axis.text.y = element_text(vjust = c(0, 0.5, 1), color = "grey60"),
    # axis.text = element_blank(),
    # axis.text = element_text(size = 7),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    panel.background = element_rect(fill = "#D9E3E6", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(hjust = 0, margin = margin(10, 0, 0, 0), color = "grey30"),
    plot.title = ggtext::element_markdown(size = 18, margin = margin(0, 0, 10, 0)),
    strip.text = element_text(face = "bold", size = 10)
  )
  
record_polaroid()
