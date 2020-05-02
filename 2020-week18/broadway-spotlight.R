library(tidyverse)
library(ggforce)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

total_grosses_df <- grosses %>% 
  group_by(show) %>% 
  summarise(total_gross = sum(weekly_gross_overall)/1e9) %>% 
  top_n(10, total_gross) %>% 
  arrange(-total_gross) %>% 
  rowwise() %>% 
  mutate(
    x1 = list(c(-total_gross, -total_gross * 1.5, total_gross / 2)),
    y1 = list(c(0, total_gross * 3.5, 0)),
    x2 = list(c(-total_gross / 1.8, -total_gross * 0.5, total_gross / 1.8)),
    y2 = list(c(0, total_gross * 3, 0)),
    x3 = list(c(-total_gross / 2, total_gross * 1.5, total_gross)),
    y3 = list(c(0, total_gross * 4, 0)),
    show_label = str_wrap(show, width = 12)
  ) %>% 
  unnest(c(x1, y1, x2, y2, x3, y3))

ggplot(total_grosses_df) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = total_gross, b = total_gross / 5, angle = 0), fill = "#C93255", colour = NA) +
  geom_polygon(aes(x1, y1), fill = "#C93255", alpha = 0.4) +
  geom_polygon(aes(x2, y2), fill = "#C93255", alpha = 0.4) +
  geom_polygon(aes(x3, y3), fill = "#C93255", alpha = 0.4) +
  geom_tile(aes(0, -12, width = total_gross * 2, height = 1), fill = "#D6AA41", colour = NA) +
  geom_text(aes(0, -19, label = paste0("$", round(total_gross, 1), "B")), hjust = 0.5, vjust = 1, colour = "#D6AA41", family = "IBM Plex Mono", size = 5) +
  geom_text(aes(0, -40, label = show_label), hjust = 0.5, vjust = 1, colour = "grey95", family = "IBM Plex Sans Bold", size = 6) +
  facet_wrap(vars(fct_reorder(show, -total_gross)), nrow = 2) +
  coord_fixed(ylim = c(-60, 90), clip = "off") +
  labs(
    title = "The Highest Grossing Broadway Shows of All Time",
    caption = "Source: Playbill | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey10", colour = "grey10"),
    plot.title = element_text(colour = "white", size = 30, family = "Avenir Next Condensed Bold", hjust = 0.5, margin = margin(20, 0, 50, 0)),
    plot.caption = element_text(colour = "#C93255", size = 8, hjust = 0.5, margin = margin(30, 0, 0, 0)),
    plot.margin = margin(20, 50, 20, 50)
  ) +
  ggsave(here::here("2020-week18", "plots", "temp", paste0("broadway-spotlight-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12.9, height = 10.877)
