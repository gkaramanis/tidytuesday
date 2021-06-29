library(tidyverse)
library(ggimage)
library(here)
library(viridis)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

invol_legal <- departures %>% 
  filter(departure_code == 4) %>% 
  count(fyear) %>% 
  rowwise() %>% 
  mutate(
    w = round(runif(1, 0, 3)),
    h = runif(1, 0, 3),
    img = here("2021", "2021-week17", "img", paste0("ceo", round(runif(1, 1, 7)), ".png"))
    ) %>% 
  ungroup()

windows <- data.frame(
  fyear = invol_legal$fyear
) %>%
  mutate(
    r = row_number(),
    c = list(c(1:4))
  ) %>% 
  unnest(c(c, r)) %>% 
  rowwise %>% 
  mutate(
    x = list(c + c(0, 0.9, 0.9, 0)),
    y = list(c * 0.5 + c(fyear - 0.45, fyear, fyear + 0.9, fyear + 0.45))
  ) %>% 
  unnest(c(x, y))

f1 = "Fira Sans Compressed"
f1l = "Fira Sans Compressed Light"
f1b = "Fira Sans Compressed Bold"
f2m = "JetBrains Mono Medium"

ggplot(invol_legal) +
  # building ----
  # roof
  annotate("polygon", x = c(-20, -12.5, -7.5, -15), y = c(2018, 2018, 2020.3, 2020.3), fill = "grey60", color = "grey20") +
  # front (years)
  annotate("polygon", x = c(-20, -12.5, -12.5, -20), y = c(1979.7, 1979.7, 2018, 2018), fill = "grey40", color = "grey20") +
  annotate("polygon", x = c(-12.5, -7.5, -7.5, -12.5), y = c(1979.7, 1982, 2020.3, 2018), fill = "grey50", color = "grey20") +
  # year labels
  annotate("text", x = -13, y = invol_legal$fyear -1.6, label = if_else(invol_legal$fyear %% 2 == 0, invol_legal$fyear, NULL), hjust = 1, color = "grey86", family = f2m, size = 4) +
  # windows ----
  geom_polygon(data = windows, aes(x = x - 12.75, y = y - 2, group = interaction(c, r)), fill = "lightblue", color = "darkblue") +
  # ceo ----
  geom_point(aes(x = -8.25 - w, y = fyear - w * 0.4), shape = 8, size = 2, color = "darkblue") +
  geom_curve(aes(x = -8.25 - w, y = fyear - w * 0.4, xend = n, yend = 1980 + h, color = fyear, size = n), curvature = -0.2) +
  geom_image(aes(x = n, y = 1980 + h - 0.5, image = img), size = 0.015, by = "height") +
  # title and annotations ----
  annotate("text", x = 22, y = 2020, hjust = 1, vjust = 1, label = "FIRED!", size = 46, family = f1b, color = "firebrick") +
  annotate("text", x = 22, y = 2012, hjust = 1, vjust = 1, label = "Involuntary CEO departures for\nlegal violations or concerns", size = 9, family = f1b, lineheight = 0.95) +
  annotate("text", x = 22, y = 2007.5, hjust = 1, vjust = 1, label = "Number of dismissals by fiscal year", size = 7, family = f1, lineheight = 0.95) +
  annotate("text", x = 20, y = 1997, hjust = 1, vjust = 1, label = str_wrap("18 CEOs were dismissed because of legal violations in 2006, and a total of 43 just between 2004 and 2006", 26), family = f1b) +
  annotate("path", x = c(17, 17, 15), y = c(1991, 1989, 1988), size = 0.3) +
  annotate("text", x = -19.25, y = 2004, angle = 90, label = "Source: Gentry et al. (2021, V03182021) · Graphic: Georgios Karamanis", size = 4, family = f1l, color = "grey80") +
  # scales, coord, theme ----
  scale_color_viridis_c(option = "inferno") +
  scale_size_continuous(range = c(0.1, 1)) +
  scale_x_continuous(breaks = 0:20, labels = ifelse(0:20 %% 5 == 0, 0:20, ""), limits = c(-20, 23)) +
  scale_y_continuous(limits = c(1979, 2021)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#CCBBEA", color = NA),
    plot.margin = margin(20, 25, 20, 25),
    axis.text.x = element_text(margin = margin(5, 0, 0, 0), family = f1),
    axis.ticks.x = element_line(color = "grey10"),
    axis.ticks.length.x = unit(0.5, "line"),
    plot.caption = element_text(hjust = 0)
  ) 

ggsave(here::here("temp", paste0("departures-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 9)

# cite article and version
