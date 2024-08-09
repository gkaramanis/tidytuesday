library(tidyverse)
library(ggcirclepack)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv')

countries <- c("United States")

olympic_men <- olympics %>% 
  filter(team %in% countries) %>%
  mutate(sex = case_when(
    str_detect(tolower(event), "women") ~ "F",
    str_detect(tolower(event), "men") ~ "M",
    TRUE ~ NA
  )) %>% 
  filter(sex == "M") %>% 
  distinct(id, name, sport, .keep_all = TRUE) %>% 
  separate(name, into = c("first", "rest")) %>% 
  count(first) %>% 
  mutate(first = fct_reorder(first, -n))

olympic_women <- olympics %>% 
  filter(team %in% countries) %>%
  mutate(sex = case_when(
    str_detect(tolower(event), "women") ~ "F",
    str_detect(tolower(event), "men") ~ "M",
    TRUE ~ NA
  )) %>% 
  filter(sex == "F") %>% 
  distinct(id, sport, .keep_all = TRUE) %>% 
  count(sport, sort = TRUE)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

nudge <- 90

ggplot() +
  # Men
  geom_circlepack(data = olympic_men, aes(id = first, area = n, fill = first == "John")) +
  geom_circlepack_text(data = olympic_men,
                       aes(id = first,
                           area = n,
                           label = if_else(n > 100, paste0(first, "\n", n), NA),
                           color = first == "John",
                       ), lineheight = 0.9, family = f1b, fontface = "bold") +
  geom_circlepack(data = olympic_women, aes(id = sport, area = n, x = after_stat(x + nudge), fill = n < 361)) +
  annotate("text", 0, -55, label = paste(scales::number(sum(olympic_men$n)), "men"), family = f1b, size = 5, color = "#31473A") +
  # Women
  geom_circlepack_text(data = olympic_women,
                       aes(id = sport,
                           area = n,
                           x = after_stat(x + nudge),
                           label = if_else(n > 50, paste0(sport, "\n", n), NA),
                           color = sport == "Athletics"
                       ), lineheight = 0.9, family = f1b, fontface = "bold") +
  annotate("text", nudge, -55, label = paste(scales::number(sum(olympic_women$n)), "women"), family = f1b, size = 5, color = "#31473A") +
  # Scales, theme, etc.
  scale_fill_manual(values = c("#D49BAE", "#BBCB50")) +
  scale_color_manual(values = c("#31473A", "#EDF4F2")) +
  coord_fixed() +
  labs(
    title = "More Johns than women swimmers",
    subtitle = str_wrap("Considering the U.S. Olympic teams from 1896 to 2016, both Summer and Winter Games included, there have been more male athletes named John than the number of women who have represented the United States in any single Olympic sport, except for athletics (track and field).", 110),
    caption = "Source: Kaggle (RGriffin) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
