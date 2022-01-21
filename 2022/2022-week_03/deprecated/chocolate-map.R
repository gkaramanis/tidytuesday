library(tidyverse)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

world <- geofacet::world_countries_grid1 %>% 
  select(name, x = col, y = row) %>% 
  mutate(name = case_when(
    name == "Viet Nam" ~ "Vietnam",
    startsWith(name, "United States") ~ "U.S.A.",
    startsWith(name, "Great Britain") ~ "U.K.",
    startsWith(name, "United Arab") ~ "U.A.E.",
    startsWith(name, "Russian") ~ "Russia",
    TRUE ~ name
  ))

# Taiwan

choc <- chocolate %>% 
  select(bean_origin = country_of_bean_origin, company_location, rating) %>% 
  filter(bean_origin != "Blend") %>% 
  add_count(bean_origin) %>% 
  filter(n > 50) %>% 
  mutate(
    company_location = case_when(
      company_location == "Amsterdam" ~ "Netherlands",
      company_location == "Scotland" ~ "U.K.",
      company_location == "Puerto Rico" ~ "U.S.A.",
      TRUE ~ company_location
    )
  ) %>%
  count(bean_origin, company_location) %>% 
  left_join(world, by = c("bean_origin" = "name")) %>% 
  left_join(world, by = c("company_location" = "name"), suffix = c("_from", "_to")) %>% 
  filter(x_from != x_to & y_from != y_to)

ggplot(choc) +
  geom_tile(aes(x_from, y_from), fill = "beige", stat = "unique") +
  geom_tile(aes(x_to, y_to), fill = "pink", stat = "unique") +
  geom_text(aes(x_from, y_from, label = bean_origin), stat = "unique", size = 2) +
  geom_text(aes(x_to, y_to, label = company_location), stat = "unique", size = 2) +
  geom_curve(aes(x = x_from, y = y_from, xend = x_to, yend = y_to, size = n, alpha = n)) +
  scale_size_continuous(range = c(0.1, 4)) +
  scale_alpha_continuous(range = c(0.1, 0.7)) +
  scale_y_reverse() +
  facet_wrap(vars(bean_origin)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )


plot_f <- function(country) {
  ggplot(choc %>% filter(bean_origin == country)) +
    geom_tile(aes(x_from, y_from), fill = "beige", stat = "unique") +
    geom_tile(aes(x_to, y_to), fill = "pink", stat = "unique") +
    geom_text(aes(x_from, y_from, label = bean_origin), stat = "unique", size = 2) +
    geom_text(aes(x_to, y_to, label = company_location), stat = "unique", size = 2) +
    geom_curve(aes(x = x_from, y = y_from, xend = x_to, yend = y_to, size = n, alpha = n)) +
    scale_size_continuous(range = c(0.1, 4)) +
    scale_alpha_continuous(range = c(0.1, 0.7)) +
    scale_y_reverse() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "grey97", color = NA)
    )
}

unique(choc$bean_origin)
