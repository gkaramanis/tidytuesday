library(tidyverse)
library(camcorder)
library(ggforce)
library(colorspace)

gg_record(dir = "temp", device = "png", width = 8, height = 8.5, units = "in", dpi = 320)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

pump_w <- pumpkins %>% 
  filter(str_detect(place, "Entries", negate = TRUE)) %>% 
  separate(id, into = c("year", "type")) %>% 
  mutate(across(c(year, weight_lbs, place, est_weight), parse_number)) %>% 
  filter(place == 1) %>% 
  count(type, country) %>% 
  mutate(type = fct_recode(type, "Field Pumpkin" = "F", "Giant Pumpkin" = "P", "Giant Squash" = "S", "Giant Watermelon" = "W", "Long Gourd" = "L", "Tomato" = "T"))
  
pump_tf <- pump_w %>% 
  complete(type, country) %>% 
  mutate(n = ifelse(!is.na(n), TRUE, FALSE)) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(
    group = case_when(
      `Giant Pumpkin` == TRUE & `Field Pumpkin` == FALSE & `Long Gourd` == FALSE ~ "p",
      `Giant Pumpkin` == FALSE & `Field Pumpkin` == TRUE & `Long Gourd` == FALSE ~ "f",
      `Giant Pumpkin` == FALSE & `Field Pumpkin` == FALSE & `Long Gourd` == TRUE ~ "l",
      `Giant Pumpkin` == TRUE & `Field Pumpkin` == TRUE & `Long Gourd` == TRUE ~ "pfl",
      `Giant Pumpkin` == FALSE & `Field Pumpkin` == TRUE & `Long Gourd` == TRUE ~ "fl"
    ),
    x = case_when(
      group == "p" ~ -1,
      group == "f" ~ 1,
      group == "l" ~ 0,
      group == "pfl" ~ 0,
      group == "fl" ~ 0.75
    ),
    y = case_when(
      group == "p" ~ 0.8,
      group == "f" ~ 0.8,
      group == "l" ~ -1,
      group == "pfl" ~ 0.2,
      group == "fl" ~ -0.25
    )
  ) %>% 
  group_by(x, y) %>% 
  summarise(group, label = paste0(country, collapse = "\n")) %>% 
  distinct()

f1 = "DIN Condensed"
f2 = "KyivType Titling"

pal =  c("#EB8914", "#8914EB", "#14EB89")

ggplot(pump_tf) +
  geom_circle(data = subset(pump_tf, group == "p" | group == "f" | group == "l"), aes(x0 = x, y0 = y, r = 1.5, fill = group), alpha = 0.5, color = NA) +
  geom_circle(data = subset(pump_tf, group == "p" | group == "f" | group == "l"), aes(x0 = x, y0 = y, r = 1.5), color = "grey20", fill = NA, size = 0.5) +
  geom_text(aes(x, y, label = label), family = f1, fontface = "bold", size = 5, color = "black") +
  annotate("text", x = c(-2, 2, -1.5), y = c(2, 2, -1.75), label = c("Giant Pumpkin", "Field Pumpkin", "Long Gourd"), family = f2, size = 7, fontface = "bold", color = darken(pal[c(3, 1, 2)], 0.5)) +
  scale_fill_manual(values = pal) +
  coord_fixed(clip = "off") +
  labs(
    title = "Big Pumpkins",
    subtitle = "Countries with the biggest pumpkins in three selected classes from 2013 to 2021",
    caption = "Source: BigPumpkins.com | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#CDD5ED", color = NA),
    plot.title = element_text(size = 34, family = f2, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 10),
    plot.margin = margin(10, 0, 10, 0)
  )

