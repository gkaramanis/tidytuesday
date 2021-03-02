library(gridfont)
library(tidyverse)
library(ggfx)

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
  
employed_sex <- employed %>% 
  filter(year == 2020) %>% 
  filter(!is.na(industry_total)) %>% 
  filter(race_gender != "TOTAL") %>% 
  filter(race_gender == "Men" | race_gender == "Women") %>% 
  distinct(industry, race_gender, industry_total) %>% 
  pivot_wider(names_from = race_gender, values_from = industry_total) %>% 
  mutate(
    total = Men + Women,
    ratio_w = Women/total,
    industry = str_replace_all(industry, "and", "&"),
    industry = str_replace_all(industry, ", &", ","),
    industry = str_replace_all(industry, "except private", "excl. priv.")
    ) %>% 
  mutate(industry = fct_reorder(industry, ratio_w)) %>% 
  arrange(ratio_w) %>% 
  mutate(n = row_number())
    

grid_employed <- employed_sex %>% 
  mutate(industry = str_remove(industry, "\n")) %>% 
  rowwise() %>% 
  summarise(create_text_df(industry, font = "smooth"), n = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(n) %>% 
  mutate(
    x_max = max(x, na.rm = TRUE),
    y_max = max(y, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    x = x/x_max * 250,
    y = y/y_max * 30
  ) %>% 
  left_join(employed_sex)

bg_col = "grey93"
pal <- c("#448D88", "#7900f1", "#DD644E") # men, women, orange

ggplot(grid_employed) +
  geom_vline(xintercept = 125, color = pal[3], size = 0.6) +
  as_reference(
    geom_rect(aes(xmin = -1.5, ymin = n * 28 + 2, xmax = ratio_w * 250, ymax = n * 28 + 30), color = NA),
    id = "women_r"
  ) +
  as_reference(
    geom_rect(aes(xmin = 251.5, ymin = n * 28 + 2, xmax = 250 - (1- ratio_w) * 250, ymax = n * 28 + 30), color = NA),
    id = "men_r"
  ) +
  with_blend(
    geom_path(aes(x, y + n * 28, group = interaction(char_idx, stroke, n), size = 1/x_max), na.rm = TRUE, color = pal[2]),
    bg_layer = "women_r",
    blend_type = "in"
  ) +
  with_blend(
    geom_path(aes(x, y + n * 28, group = interaction(char_idx, stroke, n), size = 1/x_max), na.rm = TRUE, color = pal[1]),
    bg_layer = "men_r",
    blend_type = "in"
  ) +
  geom_rect(aes(xmin = -1.5, ymin = n * 28 + 5, xmax = ratio_w * 250, ymax = n * 28 + 7), color = NA, fill = pal[2]) +
  scale_size_continuous(range = c(0.75, 1.25)) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )
