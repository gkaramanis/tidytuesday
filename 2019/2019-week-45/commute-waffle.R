library(tidyverse)
library(here)
library(waffle)
library(ggthemes)

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

# filter and add Uppsala (bike stats for April-September 2017, worker stats for 2015)
commute_people <- commute_mode %>% 
  filter(mode == "Bike") %>% 
  select(-city_size, -moe, -state_abb, -state_region, -mode, "bikers" = n) %>% 
  top_n(wt = bikers, 10) %>% 
  mutate(workers = round(bikers * 100 / percent)) %>% 
  add_row(city = "Uppsala", state = "Sweden", percent = 29, bikers = 24284, workers = 83737) %>%
  mutate(
    city = str_remove(city, " city"),
    # city = paste0(city, "\n", format(bikers, big.mark = " "))
    ) %>% 
  # clip values for big cities
  mutate(
    bikers_sort = bikers,
    non_bikers = workers - bikers,
    city = paste0(city, "\n", format(non_bikers + bikers, big.mark = " "), "\n", format(bikers, big.mark = " ")),
    # hack to make the two spaces after the city name to one, don't know where they come from, probably the big.mark option from the previous line?
    city = str_replace(city, "  ", " "),
    non_bikers_clipped = case_when(
      bikers + non_bikers > 501000 ~ 501000 - bikers,
      TRUE ~ non_bikers
    ))  %>% 
  pivot_longer(cols = c(bikers, non_bikers_clipped), names_to = "type", values_to = "n") %>%
  # scale down numbers
  mutate(
    n = n / 1000
    )

# plot
ggplot(commute_people, aes(fill = type, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(facets = ~fct_reorder(city, bikers_sort, .desc = TRUE), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = c("", "100 000", "200 000", "300 000", "400 000", "500 000\nand more"), expand = c(0,0)) +
  scale_fill_tableau(name = "One square = 1 000 workers", labels = c("commuting by bicycle", "commuting by other means")) +
  coord_equal() +
  labs(
    title = str_wrap("Uppsala, Sweden, has almost* as many workers that commute by bicyle as New York (*not really, but close)", width = 140),
    subtitle = str_wrap("Comparing Uppsala, Sweden, to the top 10 US cities with the most workers that commute by bicycle. The US data are from the ACS Survey between 2008 and 2012. The commuting data for Uppsala come from the If survey for the period between April and September 2015, and the population data from the SCB for 2015.", width = 140),
    caption = "Data: ACS Survey, If and SCB | Graphic: Georgios Karamanis",
    x = "City, total number of workers, and number of commuters by bicycle",
    y = "Number of workers"
  ) +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(
    legend.position = c(0.87, 0.9),
    panel.grid = element_blank(),
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    plot.margin = margin(20, 20, 20, 20),
    plot.subtitle = element_text(margin = margin(0, 0, 40, 0)),
    plot.caption  = element_text(margin = margin(40, 0, 0, 0)),
    strip.text.x = element_text(size = 9, margin = margin(5, 0, 15, 0), hjust = 0.5),
    plot.title = element_text(family = "IBM Plex Sans Bold")
    ) +
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave(here::here("week-45", "plots", "temp", paste0("commute-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 8)
 