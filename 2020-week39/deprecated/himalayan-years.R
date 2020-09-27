library(tidyverse)
library(colorspace)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

deaths <- members %>% 
  filter(died) %>% 
  filter(year >= 1963) %>%
  mutate(died = died * 1)

ggplot(deaths) +
  geom_text(aes(x = 0, y = 1, label = year), size = 8, family = "Helvetica Neue Condensed Black", color = "#c2b280") +
  geom_jitter(aes(x = 0, y = died), height = 0.125, width = 0.35, size = 0.5) +
  facet_wrap(vars(year), ncol = 5) +
  ylim(1.3, 0.7) +
  coord_fixed(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = darken("#c2b280", 0.2), color = NA)
  ) +
  ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 6)
