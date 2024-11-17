library(tidyverse)
library(sf)

library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 12, height = 8, dpi = 320)

countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')

pattern_analysis <- countries %>%
  select(alpha_3, name) %>%
  mutate(
    # ISO code analysis
    code_vowel_count = map_int(alpha_3, ~sum(str_count(.x, "[AEIOU]"))),
    name_vowel_count = map_int(name, ~sum(str_count(toupper(.x), "[AEIOU]"))),
    # Calculate percentages
    code_vowel_pct = (code_vowel_count / nchar(alpha_3)) * 100,
    name_vowel_pct = (name_vowel_count / nchar(name)) * 100,
    vowel_pct_difference = name_vowel_pct - code_vowel_pct
  )

world <- rnaturalearth::ne_countries(scale = 10) %>% 
  janitor::clean_names() %>% 
  select(alpha_3 = adm0_a3, admin)  %>% 
  left_join(pattern_analysis) %>% 
  st_transform(crs = "ESRI:54030")

f1 <- "Graphik"
f1b <- "Graphik Compact"

pal <- MetBrewer::met.brewer("Johnson")[2:5]

p <- ggplot(world) +
  geom_sf(aes(fill = factor(code_vowel_count)), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = pal, na.value = "grey85") +
  coord_sf(expand = FALSE) +
  labs(
    title = "Vowels in Country ISO Codes",
    subtitle = "Number of vowels (A, E, I, O, U) in each country's three-letter ISO code. Anguilla (AIA) is the only country with an ISO code containing three vowels.",
    caption = "Source: ISO 3166-1 via the ISOcodes R package · Graphic: Georgios Karamanis", 
    fill = "Number of vowels"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.margin = margin(20, 0, 20, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(family = f1b, face = "bold", size = 20),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

p +
  ggmagnify::geom_magnify(aes(from = admin == "Anguilla"), to = c(-5e6, -3.5e6, 1e6, 3e6), aspect = "fixed", linewidth = 0.1) +
  geom_text(data = NULL, aes(-4.25e6, 2.3e6, label = "Anguilla"), family = f1b, size = 3)
