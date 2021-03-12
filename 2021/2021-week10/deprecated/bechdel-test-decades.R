library(tidyverse)

bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

bechdel_dec <- bechdel %>% 
  mutate(decade = year %/% 10 * 10) %>% 
  # count(decade, rating) %>% 
  group_by(decade, rating) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(rating_string = paste(replicate(n, rating), collapse = "")) %>% 
  ungroup()

ggplot(bechdel_dec) +
  geom_text(aes(x = 0, y = -decade, label = decade), hjust = 1, nudge_x = -0.25,  stat = "unique", size = 7, family = f1, fontface = "bold") +
  geom_text(aes(x = 0 + rating * 0.25, y = -decade, label = rating, size = freq, color = n), family = f1, fontface = "bold") +
  scale_x_continuous(limits = c(-1.4, 1.4)) +
  scale_size_continuous(range = c(0, 10)) +
  scale_color_distiller(palette = "Oranges", direction = 1) +
  theme_void() +
  theme(
    # legend.position = "none",
    plot.background = element_rect(fill = "grey98", color = NA)
  ) +
  ggsave(here::here("temp", paste0("bechdel-test-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 6, width = 4)
