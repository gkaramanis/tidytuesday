library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')

eu <- country_regions %>% 
  filter(country_iso2 %in% europe_countries_grid1$code) %>% 
  left_join(all_countries) %>% 
  janitor::clean_names()

eu_meals <- eu %>% 
  filter(subcategory == "Meals") %>% 
  mutate(country_name = fct_reorder(country_name, hours_per_day_combined))
  
eu_spiral <- eu_meals %>% 
  mutate(
    country_name = case_when(
      str_detect(country_name, "Russian") ~ "Russia",
      str_detect(country_name, "United") ~ "U.K.",
      str_detect(country_name, "Bosnia") ~ "Bosnia & Herz.",
      str_detect(country_name, "Moldova") ~ "Moldova",
      str_detect(country_name, "Macedonia") ~ "N. Macedonia",
  TRUE ~ country_name
    )
  ) %>% 
  rowwise() %>% 
  mutate(
    minutes = hours_per_day_combined * 60,
    t = list(seq(pi/2, 2 * hours_per_day_combined * pi + pi/2, 0.05)),
    r = list(80 + 1.5 * t) 
    ) %>% 
  ungroup() %>% 
  unnest(c(t, r))
  
annot <- tibble(
  a = seq(pi/6, 2*pi, pi/6),
  r = 88 + 1.5 * a,
  x = r * cos(a),
  y = r * sin(a)
)

f1 <- "Outfit"

ggplot(eu_spiral) +
  geom_text(data = annot, aes(x, y, label = "┃", angle = a * 180/pi + 90), size = 1.8, color = "#758DAB") +
  geom_path(aes(x = -r * cos(t), y = r * sin(t), group = country_name, color = minutes), linewidth = 0.8) +
  shadowtext::geom_shadowtext(aes(0, 0, label = paste0(country_name, "\n", round(hours_per_day_combined * 60), "'")), family = f1, lineheight = 0.9, stat = "unique", size = 3, color = "#37434C", bg.color = "grey99", bg.r = 0.2) +
  scale_color_stepsn(colors = viridisLite::turbo(5), breaks = seq(60, 150, 15), limits = c(60, 150), guide = guide_colorsteps(title.hjust = 0.5, title.position = "top", show.limits = TRUE)) +
  labs(
    color = "Average daily time\nspent on meals* (minutes)",
    caption = paste0("Source: The Human Chronome Project · Graphic: Georgios Karamanis\n", str_wrap("*Activities centred on eating and drinking, including associated socializing. Eating and drinking. Eating meals/snacks. Pubs and restaurants. Coffee, refreshments. Meals associated with work. Visits to restaurant, café, bar.", width = 120))
  ) +
  coord_fixed(clip = "off") +
  facet_geo(vars(country_iso2), grid = "europe_countries_grid1") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2.8, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title = element_text(face = "bold", size = 16, color = "#37434C"),
    legend.text = element_text(size = 12, color = "#546472"),
    legend.margin = margin(0, 0, 30, 0),
    plot.background = element_rect(fill = "#FDFCF6", color = NA),
    strip.text = element_blank(),
    plot.margin = margin(10, 20, 10, 20),
    plot.caption = element_text(margin = margin(30, 0, 0, 0), hjust = 0.5, color = "#546472", size = 10)
  )

record_polaroid()  

