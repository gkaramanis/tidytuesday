library(tidyverse)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

total_grosses <- grosses %>% 
  group_by(show) %>% 
  summarise(total_gross = sum(weekly_gross_overall)/1e9) %>% 
  top_n(10, total_gross) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(0 , 1, total_gross/2, -total_gross/2)),
    y = list(c(10, 10, 0, 0)),
    show_label = str_wrap(show, width = 10)
    ) %>% 
  unnest(c(x, y))

 
ggplot(total_grosses) +
  geom_polygon(aes(x = x, y = y, group = show, alpha = total_gross), fill = "orange") +
  geom_text(aes(x = 0, y = 0.5, label = show_label, size = total_gross), check_overlap = TRUE, colour = "black", vjust = 0, family = "IBM Plex Serif Bold") +
  scale_size_continuous(range = c(2, 7)) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  facet_wrap(vars(show), nrow = 2) +
  theme_void(base_family = "IBM Plex Serif") +
  theme(
    plot.background = element_rect(fill = "black", colour = NA)
  ) +
  ggsave(here::here("2020-week18", "plots", "temp", paste0("broadway-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 6)


