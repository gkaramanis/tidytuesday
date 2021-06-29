library(tidyverse)
library(scales)
library(sf)
library(janitor)
library(cowplot)

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

africa <- st_read(here::here("2021", "2021-week18", "data", "afr_g2014_2013_0", "afr_g2014_2013_0.shp")) %>% 
  clean_names() %>% 
  st_simplify(dTolerance = 0.1) %>% 
  rename(country_name = adm0_name) %>% 
  mutate(
    country_name = case_when(
      country_name == "Congo" ~ "Republic of the Congo",
      country_name == "Swaziland" ~ "Eswatini",
      TRUE ~ country_name
    ))

ww <- water %>% 
  filter(!is.na(country_name)) %>% 
  filter(!country_name %in% c("Dominican Republic", "Peru", "Timor-Leste")) %>% 
  filter(water_source != "Packaged water" & !is.na(water_source)) %>% 
  mutate(
    water_source = if_else(startsWith(water_source, "Surface"), "Surface Water", water_source),
    country_name = case_when(
      country_name == "Congo - Brazzaville" ~ "Republic of the Congo",
      country_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      country_name == "Swaziland" ~ "Eswatini",
      country_name == "Tanzania" ~ "United Republic of Tanzania",
      TRUE ~ country_name
    )) %>% 
  count(country_name, water_source) %>% 
  left_join(africa) %>% 
  mutate(
    centr = st_centroid(geometry)
  )

cl <- ww %>%
  distinct(country_name, iso2) %>% 
  arrange(iso2) %>% 
  mutate(n = row_number())

pal <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")

f1 <- "Fira Sans Condensed"
f2 <- "Charter Roman"
f2b <- "Charter Bold"
f2bb <- "Charter Black"

p <- ggplot(ww) +
  geom_sf(data = africa, fill = "grey84", size = 0.2, color = "grey20") +
  geom_sf(aes(geometry = geometry, fill = n), color = "grey20", size = 0.3) +
  geom_sf_text(aes(geometry = geometry, label = iso2), size = 2, family = f1, color = "white") +
  # scale_fill_stepsn(colors = pal, n.breaks = 7, labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_fill_stepsn(colors = pal, breaks = c(10, 100, 1000, 10000), trans = "log") +
  coord_sf() +
  facet_wrap(vars(water_source)) +
  labs(
    title = "Water Sources in Africa",
    subtitle = "Number of types by country, according to data collected by WPDx",
    caption = "Source: Water Point Data Exchange · Graphic: Georgios Karamanis"
       ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(family = f1, size = 12, margin = margin(0, 0, 5, 0), color = "#58514B"),
    plot.title = element_text(hjust = 0.5, family = f2bb, size = 32, color = "#58514B"),
    plot.subtitle = element_text(hjust = 0.5, family = f2, size = 18, margin = margin(15, 0, 30, 0), color = "#58514B"),
    plot.caption = element_text(hjust = 0.5, family = f1, color = "grey30", size = 10, margin = margin(20, 0, 10, 0)),
    plot.margin = margin(20, 310, 10, 45)
  ) 

c <- ggplot(cl, aes(0, -n)) +
  geom_text(aes(label = country_name), hjust = 0, family = f1) +
  geom_text(aes(label = iso2), hjust = 1, nudge_x = -0.02, family = f1) +
  xlim(-1, 0.5) +
  ylim(-30, 0) +
  theme_void() 

ggdraw(p) +
  draw_plot(c, scale = 0.8, x = 0.15) +
  theme(
    plot.background = element_rect(fill = "grey95", color = NA)
  ) 

ggsave(here::here("temp", paste0("water-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 10)

