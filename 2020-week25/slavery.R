library(tidyverse)
library(ggforce)

# Please note that there is missing data for many of the voyages, where this database only contains details for approximately 5 million enslaved Africans who arrived alive at the final port. Many slaves died in transport, or the details around the slave voyage were not fully recorded.

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

decade_routes <- slave_routes %>% 
  mutate(
    decade = year_arrival - year_arrival %% 10
  ) %>% 
  group_by(decade) %>% 
  mutate(total = sum(n_slaves_arrived, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(decade, total) %>% 
  mutate(links_n = round(total / 10^4)) %>% 
  rowwise() %>% 
  mutate(link_y = list(c(seq(0, links_n)))) %>% 
  ungroup() %>% 
  unnest(link_y)

ggplot(decade_routes) +
  # Links I O
  geom_ellipse(aes(x0 = decade, y0 = -link_y, a = 0.65, b = 1.2 * link_y %% 2, angle = pi / 2, m1 = 3), size = 0.7, colour = "white") +
  # Shadows for I links
  geom_segment(data = subset(decade_routes, link_y %% 2 == 0), aes(x = decade - 0.6, y = -link_y - 0.65, xend = decade - 0.6, yend = -link_y + 0.65), size = 0.1, colour = "grey20") +
  geom_segment(data = subset(decade_routes, link_y %% 2 == 0), aes(x = decade + 0.6, y = -link_y - 0.65, xend = decade + 0.6, yend = -link_y + 0.65), size = 0.1, colour = "grey20") +
  annotate("text", x = c(1510, 1600, 1700, 1800, 1860), y = 3, label = c(1510, 1600, 1700, 1800, 1860), family = "DIN Condensed Bold", colour = "grey50", size = 4.5) +
  annotate("text", x = 1510, y = -23, family = "Canela Text Bold",
           label = toupper("Trans-Atlantic slave trade"),
           hjust = 0, size = 5.5, colour = "red") +
  annotate("text", x = 1510, y = -26, family = "DIN Condensed Bold",
           label = str_wrap(
             "More than 12 million enslaved Africans were forcibly transferred across the Atlantic by European colonizers over a span of 400 years. Approximately 1.2–2.4 million died during the voyage. The graphic shows the number of slaves transported by decade (of a total of about 5 million people), as documented in the records of more than 36,000 voyages between 1514 and 1866.", 88), hjust = 0, vjust = 1, size = 3.5, colour = "white") +
  annotate("text", x = 1510, y = -41, label = toupper("Source: Slave Voyages & Wikipedia | Graphic: Georgios Karamanis"), family = "IBM Plex Sans Thin", colour = "grey95", size = 2, hjust = 0, vjust = 1) +
  # Theme and stuff
  scale_y_continuous(position = "right", breaks = c(seq(0, -50, by = -10)), label = c("0", "100 000", "200 000", "300 000", "400 000", "500 000")) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey15", colour = "grey15"),
    panel.grid.major.y = element_line(colour = "grey25", size = 0.25),
    panel.grid.minor.y = element_line(colour = "grey25", size = 0.1),
    axis.text.y = element_text(colour = "red", family = "DIN Condensed Bold", hjust = 0, size = 8, margin = margin(0, 0, 0, 5)),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020-week25", "plots", "slavery.png")), dpi = 320)
