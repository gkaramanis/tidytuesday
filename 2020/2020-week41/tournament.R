library(tidyverse)
library(ggforce)
library(ggtext)
library(ggimage)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

tournament_champions <- tournament %>% 
  group_by(school) %>% 
  filter(any(tourney_finish == "Champ")) %>% 
  add_tally(tourney_finish == "Champ", name = "championships_n") %>% 
  ungroup() %>% 
  mutate(school = fct_reorder(school, -championships_n)) %>% 
  group_by(school) %>% 
  mutate(
    school_n = cur_group_id(),
    logo = paste0("2020-week41/logos/", school_n, ".png")
    ) %>% 
  ungroup()

f1 = "Produkt"
f2 = "Proxima Nova"
f2b = "Proxima Nova Bold"
f3bi = "Publico Headline Black Italic"

c1 <- "#c1c6c8" # grey
c2 <- "#005eb8" # blue
c3 <- "#002855" # dark blue

ggplot(tournament_champions) +
  annotate("segment", x = 110, xend = 1010, y = seq(1985, 2015, 5), yend = seq(1985, 2015, 5), color = c3, size = 0.2) +
  geom_tile(aes(x = 60 * school_n + 80, y = 2000, width = 40, height = 38), fill = c1, color = NA) +
  geom_linerange(aes(x = 60 * school_n + 75, ymin = 1980.9, ymax = 2019.1), color = "white", size = 0.2) +
  annotate("text", x = c(60 + 60, 60 + 75, 60 + 100, 60 + 118), y = 1980.5, label = c("60%", "75%", "100%", "TOTAL WINNING PERCENTAGE (REGULAR SEASON AND TOURNAMENT)"), vjust = 1, hjust = c(0.4, 0.4, 0.4, 0), size = 2, family = f1) +
  # Logos
  geom_image(aes(x = 60 * school_n + 80, y = 1982.2, image = logo), size = 0.03,  stat = "unique") +
  # Thick lines
  geom_link2(aes(x = 60 * school_n + full_percent, y = year, group = school, size = full_w), lineend = "round") +
  geom_text(data = subset(tournament_champions, tourney_finish == "Champ"),
             aes(x = 60 * school_n + full_percent, y = year, size = full_w, label = "★"), color = "gold", family = "Zapf Dingbats") +
  geom_text(data = subset(tournament_champions, tourney_finish == "N2nd"),
             aes(x = 60 * school_n + full_percent, y = year, size = full_w / 1.3, label = "★"), color = "lightsteelblue2", family = "Zapf Dingbats") +
  geom_text(aes(x = 60 * school_n + 80, y = 2019.2, label = str_replace(school, " ", "\n")), family = f2, size = 3.5, hjust = 0.5, vjust = 1, lineheight = 0.9, stat = "unique") +
  scale_x_continuous(limits = c(105, 1015)) +
  scale_y_reverse(breaks = seq(1985, 2015, 5), sec.axis = dup_axis()) +
  scale_size_continuous(range = c(0, 7)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "NCAA Women's DI Basketball Champions 1982-2018",
    subtitle = "Lines show total winning percentage for each year, thickness corresponds to total wins for that year.<br><span style = 'color:gold;'>★</span> Champion\t<span style = 'color:lightsteelblue2;'>★</span> Runner-up. Sorted by total number of titles.",
    caption = "Source: FiveThirtyEight | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(family = f1, color = c3),
    plot.margin = margin(20, 20, 10, 20),
    plot.title = element_text(family = f3bi, hjust = 0.5, size = 20, color = c2, margin = margin(0, 0, 7, 0)),
    plot.subtitle = element_markdown(family = f2, hjust = 0.5, size = 14, color = c3, margin = margin(0, 0, 15, 0), lineheight = 1.1),
    plot.caption = element_text(family = f1, hjust = 0.5, size = 9, color = c3, margin = margin(40, 0, 5, 0))
  ) 

ggsave(here::here("temp", paste0("tournament-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 10)
