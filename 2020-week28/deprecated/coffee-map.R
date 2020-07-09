coffee_countries = c("Ethiopia", "Guatemala", "Brazil", "Peru", "United States (Hawaii)", "Indonesia", "China", "Costa Rica", "Mexico", "Uganda", "Honduras", "Taiwan", "Nicaragua", "Tanzania", "Kenya", "Thailand", "Colombia", "Panama", "Papua New Guinea", "El Salvador", "Japan", "Ecuador", "Puerto Rico", "Haiti", "Burundi", "Vietnam", "Philippines", "Rwanda", "Malawi", "Laos", "Zambia", "Myanmar", "Mauritius", "Ivory Coast", "India")

world_map <- map_data("world", region = coffee_countries)

world_map <- map_data("world") %>% 
  filter(region != "Antarctica") %>%
  mutate(coffee = if_else(region %in% coffee_countries, TRUE, FALSE))

map <- ggplot(world_map) +
  geom_polygon(aes(long, lat, group = group, fill = coffee)) +
  scale_fill_manual(values = c(darken("#6f4e37", 0.3), "#c19d67")) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#6f4e37", colour = NA)
  )
