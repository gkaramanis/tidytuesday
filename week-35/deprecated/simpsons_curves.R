library(tidyverse)
library(here)
library(ggalt)

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

sim <- simpsons %>%
  # I want no movie
  filter(season != "Movie") %>%
  add_count(guest_star) %>%
  filter(n > 1) %>%
  separate(number, c("episode_n", "se"), sep = "–") %>%
  mutate_at(c("season", "se"), as.numeric) %>%
  mutate(se = se - season * 100)

ggplot(data = sim) +
  geom_text(aes(label = season, x = -0.1, y = season), hjust = 1, family = "IBM Plex Mono Thin", alpha = 0.1) +
  geom_text(aes(label = guest_star, x = 0, y = season), hjust = 0, family = "IBM Plex Mono Thin", alpha = 0.1) +
  xlim(-0.5, 10) +
  scale_y_reverse() +
  theme_void() +
  theme(
    plot.background = element_rect()
  ) 

ggsave(
    here::here("week-35", "figures", "temp", paste0("simpson_curves_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    height = 10, width = 4, dpi = 320
 )