library(tidyverse)
library(waffle)
library(MetBrewer)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')

items <- loadouts %>% 
  group_by(season) %>% 
  mutate(item = fct_lump_min(item, 10)) %>%
  count(season, item) %>% 
  mutate(
    total = sum(n),
    pct = round(n / total * 100, 1)
    ) %>% 
  arrange(-pct) %>% 
  mutate(
    rank = row_number(),
    item_id = as.numeric(item),
    color = rev(met.brewer("Redon"))[item_id],
    color = if_else(item == "Other", "grey70", color),
    season_label = paste0("<span style='color:grey50'>Season</span> ", season)
    ) %>% 
  ungroup()

f1 <- "Outfit"
f2 <- "Outfit Light"

p <- ggplot(items, aes(fill = color, values = n)) +
  geom_waffle(flip = TRUE) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(vars(season_label), strip.position = "bottom") +
  coord_fixed() +
  labs(
    title = "ALONE",
    subtitle = "Number of times each item was used per season. 'Other' includes all items used 9 times or less",
    caption = "Source: Alone data package 🐻 Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#FFFFF9", color = NA),
    strip.text = element_markdown(size = 20, hjust = 0.1, family = f2, color = "#1F3147"),
    plot.title = element_markdown(size = 56, face = "bold", hjust = 0.5, fill = "#1F3147", color = "white", padding = unit(c(1.5, 20, 0.5, 20), "line"), margin = margin(-6, 0, 0, 0)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30", margin = margin(10, 0, 0, 0)),
    plot.caption = element_text(margin = margin(30, 0, 10, 0), hjust = 0.5, color = "#1F3147")
  )
  
items_y <- layer_data(p) %>% 
  mutate(panel = as.numeric(PANEL)) %>% 
  select(fill, y, panel, group) %>% 
  left_join(items, by = c("fill" = "color", "panel" = "season")) %>% 
  group_by(season = panel, item) %>% 
  summarise(y = mean(y), n, color = fill) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(season_label = paste0("<span style='color:grey50'>Season</span> ", season)
  )

p +
  geom_text(data = items_y, aes(x = 11, y = y, label = paste(n, item), color = colorspace::darken(color, 0.3)), hjust = 0, family = f1, fontface = "plain")

