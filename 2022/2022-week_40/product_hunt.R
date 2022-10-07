library(tidyverse)
library(emo)
library(lubridate)
library(waffle)
library(patchwork)
library(shadowtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')


ph_emoji <- product_hunt %>% 
  mutate(
    year = year(release_date),
    has_emoji = ji_detect(product_description),
    emoji = ji_extract_all(product_description)
    ) %>% 
  select(year, has_emoji, emoji) %>% 
  # Filter products with emoji in description
  filter(has_emoji) %>% 
  unnest(emoji) %>% 
  # Calculations
  group_by(year, emoji) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    total = sum(n),
    p = n / total
    ) %>% 
  ungroup()

# Top 5 by proportion
top_emoji <- ph_emoji %>% 
  group_by(year) %>% 
  slice_max(order_by = p, n = 3)

# Count all products by year
ph_year <- product_hunt %>% 
  mutate(year = year(release_date)) %>% 
  count(year)

# ph_emoji %>% 
#   filter(emoji == "✅") %>%  
#   mutate(cum_n = cumsum(n))
# 
# ph_emoji %>% 
#   distinct(year, total) %>% 
#   mutate(cum_total = cumsum(total))

f1 <- "Outfit"

p1 <- ggplot(ph_emoji, aes(fill = emoji, values = n, label = emoji)) +
  geom_pictogram(n_rows = 20, flip = TRUE, size = 1.2) +
  scale_y_continuous(breaks = seq(0, 150, 25), labels = function(x) scales::number(x * 20), expand = c(0,0), position = "right") +
  facet_wrap(vars(year), nrow = 1, strip.position = "bottom") +
  labs(
    title = "💥Emojiplosion💥",
    subtitle = str_wrap("The use of emoji in the product descriptions on Product Hunt exploded in 2018. The waffle chart shows all the emoji used by year, from a total of 9 emoji in 7 500 product descriptions in 2014 to 2 843 emoji in 2021. The most used emoji, the check mark button ✅ , has been used 872 times, more than two times the number of all emoji used between 2014 and 2017.", 110),
    caption = "Source: components.one · Graphic: Georgios Karamanis"
  ) +
  coord_fixed() +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(lineheight = 1, margin = margin(0, 0, -40, 0))
  )
  
p2 <- ggplot(ph_year, aes(year, n, label = paste(round(n / 1000, 1), "K"))) +
  geom_line(color = "gray60") +
  geom_point(color = "gray60") +
  geom_shadowtext(nudge_y = 500, family = f1, color = "gray60", bg.color = "gray99", bg.r = 0.2, size = 3) +
  scale_x_continuous(breaks = 2014:2021) +
  scale_y_continuous(labels = scales::label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "Total number of products by year") +
  theme_void(base_family = f1, base_size = 8) +
  theme(
    axis.text.x = element_text(color = "gray60"),
    plot.title = element_text(color = "gray40")
  )

p3 <- top_emoji %>% 
  arrange(year, -p) %>% 
  group_by(year) %>% 
  summarise(label = paste(emoji, collapse = " ")) %>% 
  ggplot() +
  geom_text(aes(-0.7, year, label = paste(year, "  ")), hjust = 0.5, family = f1, color = "gray60", size = 3) +
  geom_text(aes(0, year, label = label), hjust = 0) +
  scale_x_continuous(limits = c(-1, 10)) +
  scale_y_reverse() +
  coord_cartesian(clip = "off") +
  labs(title = "Top 3 (with ties) emoji by frequency of use") +
  theme_void(base_family = f1) +
  theme(
    plot.title = element_text(margin = margin(0, 0, 10, 0), size = 10, color = "gray40")
  )

p1 +
  inset_element(p2, 0.03, 0.62, 0.45, 0.87) +
  inset_element(p3, 0.03, 0.18, 0.4, 0.48)

