library(tidyverse)
library(colorspace)

bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

f1 = "JetBrains Mono"
f1l = "Regular"
f1b = "ExtraBold"

bechdel_med <- bechdel %>% 
  group_by(year) %>% 
  summarise(median_rating = median(rating), n = n()) %>% 
  mutate(
    char = strsplit(as.character(year), split = ""),
    char_n = list(c(0:3))
  ) %>% 
  ungroup() %>% 
  unnest(c(char, char_n)) %>% 
  mutate(
    fam = paste0(f1, " ", if_else(char_n == floor(median_rating) | char_n == ceiling(median_rating), f1b, f1l)),
    decade = (year - 1) %/% 10 * 10,
    x = year - decade,
    margin_x = if_else(lag(str_detect(fam, "Bold"), default = FALSE), 0.04, 0)
  ) %>% 
  group_by(year) %>% 
  mutate(margin_x = cumsum(margin_x)) %>% 
  ungroup()

ggplot(bechdel_med) +
  geom_text(aes(x = x + char_n/10 + margin_x, y = decade, 
                label = char, family = fam,
                size = if_else(str_detect(fam, "Bold"), 4.5, 3.5),
                color = if_else(str_detect(fam, "Bold"), n, NULL)
                ),
            stat = "unique", hjust = 0) +
  # Legend
  annotate("text", x = c(1, 1.15, 1.35, 1.55), y = 1850, label = c("1", "9", "7", "6"), size = c(6, 6, 7.5, 6), family = c("JetBrains Mono Regular", "JetBrains Mono Regular", "JetBrains Mono ExtraBold", "JetBrains Mono Regular"), color = c("grey20", "grey20", 2, "grey20")) +
  annotate("text", x = c(1, 1.15, 1.35, 1.55), y = 1862, label = c("0", "1", "2", "3"), size = 3, family = "JetBrains Mono Regular", color = "grey20") +
  annotate("text", x = 1.95, y = 1860.5, label = "Bold position indicates the median of Bechdel test rating\nfor the year (two bold numbers mean a median between them)", size = 2.5, family = "JetBrains Mono Regular", color = "grey10", lineheight = 0.9, hjust = 0, vjust = 1) +
  annotate("segment", x = c(1, 1.15, 1.35, 1.55), y = 1859,
           xend = c(1, 1.15, 1.35, 1.55), yend = 1855,
           color = darken("#5E92B3", 0.2), size = 0.2, arrow = arrow(length = unit(0.005, "npc"))) +
  # Title
  annotate("text", x = 10.5, y = 1850, label = "Bechdel Test", size = 11, family = "Graphik Semibold", hjust = 1) +
  annotate("text", x = 10.5, y = 1859, label = "Median test rating by year for 8 839 films\nbetween 1888 and 2021", size = 3.2, family = "Graphik", hjust = 1, vjust = 1, lineheight = 1) +
  # Scales, theme, etc
  scale_y_reverse() +
  scale_size_identity() +
  scale_color_distiller(palette = "Reds", na.value = "grey20", direction = 1, guide = guide_colorbar(title = "Color indicates total number of films for the year", title.position = "top", title.vjust = 1)) +
  # Caption
  labs(
    caption = "Source: Bechdeltest.com · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.35, 0.95),
    legend.direction = "horizontal",
    legend.key.height = unit(0.25, "line"),
    legend.key.width = unit(3.35, "line"),
    legend.title = element_text(family = "JetBrains Mono Regular", size = 7, color = "grey10", hjust = 0),
    legend.text = element_text(family = "JetBrains Mono Regular", size = 7, color = "grey10"),
    plot.background = element_rect(fill = lighten("#5E92B3", 0.3), color = NA),
    plot.caption = element_text(family = "Graphik Light", hjust = 0.92, size = 7.5, margin = margin(0, 0, 10, 0)),
    plot.margin = margin(10, 0, 0, 10)
  ) +
  ggsave(here::here("temp", paste0("bechdel-test-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 6, width = 8)

