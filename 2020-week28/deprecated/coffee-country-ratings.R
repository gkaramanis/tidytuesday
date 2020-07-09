country_ratings <- coffee_ratings %>% 
  filter(!is.na(country_of_origin)) %>% 
  mutate(
    country = case_when(
      str_detect(country_of_origin, "Puerto") ~ "Puerto Rico",
      str_detect(country_of_origin, "Tanzania") ~ "Tanzania",
      str_detect(country_of_origin, "Cote") ~ "Ivory Coast",
      TRUE ~ country_of_origin
    ),
    continent = countrycode(country, origin = "country.name", destination = "continent")
  ) %>% 
  group_by(country) %>% 
  mutate(tcp_med = median(total_cup_points)) %>% 
  ungroup() %>% 
  distinct(country, continent, tcp_med) %>% 
  mutate(country = if_else(str_detect(country, "Hawaii"), "Hawaii", country)) %>% 
  group_by(continent) %>% 
  # mutate(country = fct_reorder(country, tcp_med, .desc = TRUE)) %>% 
  arrange(-tcp_med) %>% 
  mutate(n = row_number()) %>% 
  ungroup()

ggplot(country_ratings) +
  geom_bar(aes(tcp_med, -n), stat="identity", orientation = "y", width = 0.1,  fill = lighten("#c19d67", 0.5)) +
  geom_text(aes(1, -n + 0.15, label = country),
            hjust = 0, vjust = 0, family = "DIN Condensed Bold", size = 3, colour = lighten("#c19d67", 0.5)) +
  facet_wrap(vars(continent), ncol = 4) +
  theme_void(base_family = "DIN Condensed Bold", base_size = 18) +
  theme(
    strip.text = element_text(colour = "#c19d67"),
    plot.background = element_rect(fill = "#6f4e37", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020-week28", "plots", "temp", paste0("coffee-ratings-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 11, height = 4)
