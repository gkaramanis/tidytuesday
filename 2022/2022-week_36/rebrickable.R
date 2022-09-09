library(tidyverse)
library(camcorder)
library(geomtextpath)
library(ggtext)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 6, units = "in", dpi = 320)

elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')

cols <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')

sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')

inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')

themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

cols_yrs <- inventories %>% 
  left_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  left_join(cols, by = c("color_id" = "id")) %>% 
  left_join(inventories, by = "set_num") %>% 
  left_join(sets, by = "set_num") %>% 
  mutate(hex = case_when(
    !is.na(rgb) ~ paste0("#", rgb),
    TRUE ~ rgb
  )
  ) 

cols_freq <- cols_yrs %>% 
  count(year, name.x, hex) %>% 
  filter(year > 1969) %>%
  filter(!is.na(hex)) %>% 
  group_by(year) %>% 
  mutate(
    total = sum(n),
    freq = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(
    # str_detect(name.x, "Light Gray|Light Bluish Gray|Dark Gray|Dark Bluish Gray")
    name.x == "Light Gray" |
      name.x == "Light Bluish Gray" |
    name.x == "Dark Gray" |
      name.x == "Dark Bluish Gray"
         )

col_labels <- cols_freq %>% 
  distinct(name.x, hex) %>% 
  mutate(
    x = c(1970, 1979, 2021, 2022),
    y = c(0.07, 0.03, 0.05, 0.18),
    hjust = c(0, 0, 1, 1)
  )

f1 <- "Outfit"
tx_col <- "darkslateblue"

ggplot(cols_freq) +
  geom_vline(xintercept = 2003.5, color = "coral2", size = 0.75, alpha = 0.7) +
  geom_line(aes(year, freq, color = hex, linetype = if_else(str_detect(name.x, "Blu"), "solid", "4141")), size = 1.4, alpha = 1) +
  geom_text(data = col_labels, aes(x, y, label = name.x, hjust = hjust, color = hex), family = f1, fontface = "bold") +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0.1, 0.3, 0.1), labels = scales::percent) +
  labs(
    title = "The LEGO gray switch",
    subtitle = "In <span style = 'color:#EE6A50'>2004</span>, LEGO changed <span style = 'color:#9BA19D'>◼**light gray**</span> to <span style = 'color:#A0A5A9'>◼**light bluish gray**</span> and <span style = 'color:#6D6E5C'>◼**dark gray**</span> to <span style = 'color:#6C6E68'>◼**dark bluish gray**</span>. The chart shows<br>the proportion of parts with the four gray colors by year, relative to parts of all colors released each year.",
    caption = "Source: Rebrickable · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(color = tx_col, face = "bold"),
    plot.title = element_text(size = 20, color = tx_col, face = "bold"),
    plot.subtitle = element_markdown(size = 12, margin = margin(7, 0, 30, 0), color = tx_col, lineheight = 1.2),
    plot.caption = element_text(margin = margin(20, 0, 0, 0), color = tx_col),
    plot.margin = margin(20, 20, 10, 20)
  )
  
