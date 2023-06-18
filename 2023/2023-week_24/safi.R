library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 5, units = "in", dpi = 320)

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')


safi_m <- safi_data %>% 
  select(instanceID, village, months_lack_food) %>% 
  rowwise() %>% 
  mutate(months = as.list(strsplit(months_lack_food, ";"))) %>% 
  ungroup() %>% 
  unnest(months) %>% 
  # mutate(months = if_else(months == "none", NA, months)) %>% 
  filter(months != "none") %>% 
  add_count(village, months) %>% 
  mutate(
    months = substr(months, 1, 3),
    months = fct_relevel(months, month.abb)
    ) %>% 
  distinct(village, months, n) %>%
  mutate(n = as.double(n)) %>% 
  complete(village, months, fill = list(n = 0.3)) %>% 
  arrange(village, months) %>% 
  group_by(village) %>%
  mutate(
    pct = n/sum(n),
    ymax = cumsum(pct),
    ymin = lag(ymax, default = 0),
    laby = (ymin + ymax) / 2
  ) %>%
  ungroup() 

f1 <- "Outfit"
f2 <- "Andada Pro"
   
ggplot(safi_m) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = 2.7, xmax = 4, fill = if_else(n != 0.3, months, NA)), color = "white") +
  shadowtext::geom_shadowtext(data = . %>% filter(n != 0.3), aes(x = 3.35, y = laby, label = paste0(months, if_else(n > 9, "\n", " "), "(", n, ")"), size = pct), family = f1, color = "white", lineheight = 0.8, bg.color = alpha("grey20", 0.8)) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_fill_manual(values = cetcolor::cet_pal(13, name = "c1"), na.value = "black") +
  scale_size_continuous(range = c(2.5, 5)) +
  facet_wrap(vars(village)) +
  labs(
    title = "Which months, in the last 12 months, have you faced a situation when you did not have enough food to feed the household?",
    subtitle = "Number of responses for each month by village",
    caption = "Source: SAFI survey · Graphic: Georgios Karamanis"
  ) +
  coord_polar(theta = "y") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(family = f2, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(family = f2, hjust = 0.5, margin = margin(5, 0, 20, 0), size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )
    
