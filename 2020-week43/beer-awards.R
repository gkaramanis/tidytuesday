library(tidyverse)
library(waffle)
library(cowplot)
library(ggtext)

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

beer_medals <- beer_awards %>% 
  mutate(medal = fct_relevel(medal, "Gold", "Silver", "Bronze")) %>% 
  count(year, medal) %>% 
  filter(year == min(beer_awards$year) | year == max(beer_awards$year))

categories <- beer_awards %>% 
  filter(year == min(beer_awards$year) | year == max(beer_awards$year)) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(category))

a <- ggplot() +
  geom_richtext(data = NULL, aes(x = 0, y = 0, label = "In 1987 there were<br>awarded 27 <span style = 'color:#E4B44D;'>gold</span>,<br><span style = 'color:#8B8D91;'>silver</span> and <span style = 'color:#AB7561;'>bronze</span><br>medals across 12<br>categories, while<br>in 2020 there were<br>awarded 218 medals<br>in 90 categories"), hjust = 0, vjust = 1, family = "Helvetica", size = 7, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  theme_void()

p <- ggplot(beer_medals) +
  geom_pictogram(aes(values = n, label = "beer", color = medal), n_rows = 10, flip = TRUE, family = "Font Awesome 5 Free Solid", size = 6) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(5.8, 20.8, 5), labels = function(x) x * 10 - 8, expand = c(0, 0), position = "right") +
  scale_label_pictogram(name = NULL, values = c("beer"), labels = c("beer")) +
  scale_color_manual(breaks = c("Gold", "Silver", "Bronze"), values = c("#E4B44D", "#8B8D91", "#AB7561")) +
  coord_fixed(clip = "off") +
  labs(
    title = toupper("Medal inflation"),
    subtitle = "at the Great American Beer Festival",
    caption = "Source: Great American Beer Festival | Graphic: Georgios Karamanis "
       ) +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    strip.text = element_text(size = 14, family = "Atkinson Hyperlegible Bold"),
    plot.title = element_text(size = 40, family = "Graphik Compact Bold", hjust = 0.5, margin = margin(5, 0, 10, 0)),
    plot.subtitle = element_text(size = 25, family = "Atkinson Hyperlegible", hjust = 0.5, margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 8, family = "Atkinson Hyperlegible", hjust = 0.5, margin = margin(15, 0, 0, 0))
  ) +
  facet_wrap(vars(year), nrow = 1, strip.position = "bottom")

ggdraw(p) +
  draw_plot(a, x = -0.42, y = 0.23) 

ggsave(here::here("temp", paste0("beer-awards-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8.5, width = 7)

