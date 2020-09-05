library(tidyverse)
library(janitor)
library(colorspace)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

continents_2018 <- key_crop_yields %>% 
  clean_names() %>% 
  filter(is.na(code)) %>%
  filter(entity == "Africa" | entity == "Americas" | entity == "Asia" | entity == "Europe" | entity == "Oceania") %>% 
  filter(year == 2018) %>% 
  rename_with(., ~ str_remove(.x, "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = wheat:bananas, names_to = "crop") %>% 
  group_by(entity) %>% 
  mutate(
    total = sum(value, na.rm = TRUE),
    pct = value/total,
    ent_n = cur_group_id()
  ) %>% 
  slice_max(pct, n = 3) %>% 
  ungroup() %>% 
  group_by(entity) %>% 
  mutate(crop_n = row_number())
    

totals <- continents_2018 %>% 
  distinct(entity, total) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(0, 0, total, total)),
    y = list(c(0, total, total, 0)),
    isox_bottom = list(x - y),
    isoy_bottom = list((x + y) / 2),
    isox_top = list(isox_bottom),
    isoy_top = list(isoy_bottom + total),
    isox = list(c(isox_bottom[c(2, 1, 4)], isox_top[c(4, 3, 2)])),
    isoy = list(c(isoy_bottom[c(2, 1, 4)], isoy_top[c(4, 3, 2)]))
  )  %>% 
  ungroup() %>% 
  unnest(c(isox, isoy)) %>% 
  select(-isox_bottom, -isoy_bottom, -isox_top, -isoy_top) %>% 
  mutate(
    x = case_when(
      entity == "Europe" ~ 50,
      entity == "Africa" ~ 0,
      entity == "Asia" ~ 180,
      entity == "Oceania" ~ 100,
      entity == "Americas" ~ -100,
    ),
    y = case_when(
      entity == "Europe" ~ -25,
      entity == "Africa" ~ -50,
      entity == "Asia" ~ -60,
      entity == "Oceania" ~ -125,
      entity == "Americas" ~ 0,
    )
  ) %>%
  mutate(entity = fct_relevel(entity, c("Americas", "Europe", "Africa", "Asia", "Oceania"))) %>% 
  group_by(entity) %>% 
  mutate(label_y = max(isoy)) %>% 
  ungroup()

f1 <- "Canela Text"
f1b <- "Canela Text Bold"
f2 <- "Proxima Nova"
f2b <- "Proxima Nova Bold"
f3 <- "IBM Plex Sans Condensed Medium"
f3b <- "IBM Plex Sans Condensed Bold"

pal <- c("#47607E", "#96A7B2", "#6788A5", "#D7664B", "#20527D")

ggplot(totals) +
  geom_polygon(aes(x = isox + x, y = isoy + y, group = entity, fill = entity, color = entity), size = 1) +
  geom_text(aes(x = x - 20, y = y + label_y + 44, label = entity), hjust = 1, stat = "unique", family = f1b, size = 5) +
  geom_text(aes(x = x - 20, y = y + label_y + 30, label = paste0(round(total, 1), " tonnes/ha")), hjust = 1, stat = "unique", family = f1, size = 4.5, lineheight = 0.9) +
  geom_segment(aes(x = x - 20, y = y + label_y + 21, xend = x - 20, yend = y + label_y - 20), size = 0.15) +
  geom_segment(aes(x = x - 35, y = y + label_y + 21, xend = x - 20, yend = y + label_y + 21), size = 0.15) +
  
  geom_text(data = continents_2018, aes(x = ent_n * 100 - 300, y = 306, label = entity), family = f1b, hjust = 0, stat = "unique") +
  geom_text(data = continents_2018, aes(x = ent_n * 100 - 300, y = 305 - crop_n * 10, label = crop), family = f2, hjust = 0) +
  geom_text(data = continents_2018, aes(x = ent_n * 100 - 235, y = 305 - crop_n * 10, label = format(round(pct * 100, 1), nsmall = 1)), family = f3b, hjust = 1, color = "#D76549") +
  geom_segment(aes(x = -200, y = 263, xend = 266, yend = 263)) +
  annotate("rect", xmin = -202, ymin = 320, xmax = -143, ymax = 330, fill = "black") +
  annotate("text", x = -200, y = 325, hjust = 0, label = toupper("Top 3 key crops"), family = f3, color = "#EFE1C7", size = 3) +
  annotate("text", x = -138, y = 325, hjust = 0, label = toupper("by percentage of total key crop yield"), family = f3, size = 3, color = "black") +
  
  annotate("text", x = -115, y = -90, label = "2018", size = 30, family = f1b) +
  annotate("text", x = -115, y = -135, label = toupper("Total key crop yield"), size = 6.5, family = f3b) +
  
  labs(caption = toupper("Source: Our World in Data | Graphic: Georgios Karamanis")) +
  
  scale_fill_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), values = pal) +
  scale_color_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), values = darken(pal)) +
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#EFE1C7", color = NA),
    plot.margin = margin(20, 22, 20, 22),
    plot.caption = element_text(family = f2, size = 8, hjust = 1)
  ) +
  ggsave(here::here("temp", paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 9)
