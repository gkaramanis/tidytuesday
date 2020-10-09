library(tidyverse)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

tournament_decade <- tournament %>% 
  mutate(decade = floor(year / 10) * 10)

champions <- tournament_decade %>% 
  filter(tourney_finish == "Champ")

pal <- c("#2f4f4f", "#a0522d", "#006400", "#000080", "#ff0000", "#00ced1", "#ffa500", "#ffff00", "#00ff00", "#0000ff", "#d8bfd8", "#ff00ff", "#1e90ff", "#98fb98", "#ff69b4")

f1 = "Produkt"
f2b = "Proxima Nova Bold"

ggplot(tournament_decade) +
  geom_point(aes(x = full_w, y = -full_l), color = "#c1c6c8", size = 1, alpha = 0.2) +
  geom_point(data = champions, aes(x = full_w, y = -full_l, color = school)) +
  geom_text(data = champions, aes(x = 15, y = -23 - 2 * (year - decade), label = paste0(year, " ", school), color = school), hjust = 0, family = f1) +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(decade), nrow = 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(family = f2b, size = 18, color = "#005eb8", margin = margin(5, 0, 7.5, 0)),
    strip.background = element_rect(fill = "grey93", color = NA),
    panel.background = element_rect(fill = "grey97", color = NA),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  ) 
  # ggsave(here::here("temp", paste0("tournament-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 10, height = 6)
