library(tidyverse)
library(tidytext)
library(geofacet)
library(treemapify)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

hp_n <- haunted_places %>% 
  unnest_tokens(word, description) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(state, state_abbrev, word) %>% 
  group_by(state) %>% 
  slice_max(order_by = n, n = 3) %>% 
  mutate(i = row_number()) %>% 
  ungroup() %>% 
  mutate(emoji = case_when(
             word == "night" ~ "🌙",
             word == "people" ~ "👥",
             word == "road" ~ "🛣️",
             word == "girl" ~ "👧",
             word == "ghost" ~ "👻",
             word == "haunted" ~ "🏚️",
             word == "heard" ~ "👂",
             word == "hear" ~ "👂",
             word == "building" ~ "🏢",
             word == "floor" ~ "🟫",
             word == "bridge" ~ "🌉",
             word == "woman" ~ "👩",
             word == "cemetery" ~ "🪦",
             word == "school" ~ "🏫",
             word == "house" ~ "🏠",
             word == "bldg" ~ "🏢",
             word == "hall" ~ "🏛️",
             word == "church" ~ "⛪",
             word == "girls" ~ "👭",
             word == "walking" ~ "🚶‍♀️"
  ),
  word = case_when(
    word == "building" | word == "bldg" ~ "building",
    word == "hear" | word == "heard" ~ "hear",
    TRUE ~ word
  )
  )

lgnd <- hp_n %>% 
  distinct(word, emoji) %>%
  arrange(word) %>% 
  mutate(i = row_number()) 

f1 <- "Outfit"
f2 <- "DIN Condensed"
f3 <- "Newsreader Text"

l <- ggplot(lgnd) +
  geom_text(aes(0, 0, label = paste(emoji, " ", word, " ")), hjust = 0, family = f2, color = "orange2") +
  facet_wrap(vars(i), nrow = 2) +
  coord_cartesian(clip = "off") +
  theme_void()

p <- hp_n %>% 
  mutate(state = if_else(str_detect(state, "DC"), "District of Columbia", state)) %>% 
  ggplot(aes(area = n, label = emoji, family = f1)) +
  geom_treemap(aes(fill = n), color = "black", start = "topleft") +
  geom_treemap_text(place = "center", start = "topleft") +
  geom_treemap_text(aes(label = n), place = "topleft", start = "topleft", size = 4, color = "white") +
  scale_fill_gradient(low = "purple4", high = "coral3") +
  facet_geo(vars(state)) +
  theme_gray(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA),
    plot.margin = margin(10, 10, 50, 0),
    strip.text = element_text(family = f2, margin = margin(2, 0, 2, 0), color = "white"),
    strip.background = element_rect(fill = "purple")
  )

# To remove empty facets from p
p_grob <- get_geofacet_grob(p)

# Add empty plot and set its width to 0
# (Returns NULL without it?)
ggplot() + p_grob +
  plot_layout(widths = c(0, 1)) +
  inset_element(l, left = 0.06, right = 0.84, bottom = -0.03, top = 0.07) +
  plot_annotation(
    title = "Haunted places",
    subtitle = str_wrap("Top 3 words mentioned in The Shadowlands Haunted Places Index by state. The size of the rectangles represents the share of the words within each state. The number and color indicate how many times each word is mentioned.", 110),
    caption = "Source: The Shadowlands Haunted Places Index · Graphic: Georgios Karamanis"
  ) & 
  theme_void(base_family = f1) +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey15", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(color = "orange", face = "bold", size = 28, family = f3),
    plot.subtitle = element_text(color = "orange2", size = 12, margin = margin(5, 0, 15, 0), lineheight = 1),
    plot.caption = element_text(color = "orange3", hjust = 0.5, margin = margin(10, 0, 0, 0), size = 10)
  )
  
  

