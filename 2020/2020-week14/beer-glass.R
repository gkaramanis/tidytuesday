library(spatstat)
library(tidyverse)
library(ggtext)

beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')

beer_production <- beer_taxed %>% 
  group_by(year) %>% 
  summarise(
    bar_keg = sum(month_current[type == "In barrels and kegs" | type == "In kegs"]),
    bot_can = sum(month_current[type == "In bottles and cans"]),
    production = bar_keg + bot_can
  ) 

beer_hex <- beer_production %>% 
  mutate(
    n_total = round(760 * production / max(production)),
    n_bot_can = round(bot_can / production * n_total),
    n_bar_keg = n_total - n_bot_can,
    n_na = 781 - n_total
  ) %>% 
  select(year, starts_with("n_")) %>% 
  pivot_longer(n_bot_can:n_na, names_to = "type", values_to = "value") %>%
  uncount(value) %>% 
  select(-n_total) %>% 
  group_by(year) %>% 
  mutate(n = row_number()) %>%
  group_split() %>% 
  as.data.frame() %>% 
  select(n, starts_with("type")) %>% 
  set_names(c("n", paste0("year_", 2008:2019)))

beer_glass <- read_csv(here::here("2020/2020-week14", "data", "beer.csv")) %>% 
  mutate(
    x = round(x),
    y = round(400 - y)
    )

hex_glass <- data.frame(hextess(owin(poly = beer_glass), 10, trim = FALSE)) %>% 
  select(x, y, Tile) %>% 
  group_by(Tile) %>% 
  mutate(
    xm = mean(x),
    ym = mean(y)
  ) %>% 
  ungroup() %>% 
  arrange(ym, xm) %>% 
  group_by(ym, xm) %>% 
  mutate(n = group_indices()) %>% 
  ungroup() %>% 
  left_join(beer_hex) %>% 
  pivot_longer(year_2008:year_2019, names_to = "year", values_to = "type") %>%
  mutate(year = str_remove(year, "year_"))
  
ggplot(hex_glass) +
  geom_polygon(aes(x, y, group = Tile, fill = type), colour = NA) +
  geom_text(data = beer_production, aes(x = 200, y = -440, label = paste0(round(bar_keg/1000000, 1), " M")), family = "IBM Plex Mono Bold", hjust = 0.5, size = 4, colour = "white", check_overlap = TRUE) +
  geom_text(data = beer_production, aes(x = 200, y = -485, label = paste0(round(bot_can/1000000, 1), " M")), family = "IBM Plex Mono Bold", hjust = 0.5, size = 4, colour = "#f28e1c",  check_overlap = TRUE) +
  scale_fill_manual(values = c("grey97", "#f28e1c", "grey50")) +
  coord_fixed() +
  facet_wrap(vars(year), nrow = 2) +
  labs(
    title = "Beer produced in <span style='color:white'>kegs</span><br>and <span style='color:#f28e1c'>bottles and cans</span>",
    subtitle = "Million barrels per year",
    caption = "Source: Alcohol and Tobacco Tax and Trade Bureau | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Mono", base_size = 24) +
  theme(
    legend.position = "none",
    strip.text = element_text(colour = "grey97", family = "American Typewriter Semibold", margin = margin(20, 0, 10, 0)),
    plot.background = element_rect(fill = "grey80", colour = NA),
    plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, family = "American Typewriter Bold", margin = margin(10, 0, 10, 0)),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "grey35", family = "IBM Plex Sans Bold", margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, colour = "white", family = "American Typewriter", size = 11, margin = margin(30, 0, 10, 0)),
    plot.margin = margin(20, 22, 20, 22)
  ) 

ggsave(here::here("2020-week14", "plots", "beer-glass.png"), dpi = 320, width = 10.8, height = 12)
  