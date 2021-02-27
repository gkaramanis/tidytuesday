library(gridfont)
library(tidyverse)

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
    industry = str_replace_all(industry, "except private", "excl. priv."),
    n = row_number()
    )

grid_employed <- employed_sex %>% 
  mutate(industry = str_remove(industry, "\n")) %>% 
  rowwise() %>% 
  summarise(create_text_df(industry, font = "smooth"), n = cur_group_id()) %>% 
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
  left_join(employed_sex) %>% 
  arrange(-ratio_w)

bg_col = "grey93"
pal <- c("#448D88", "#7900f1", "#DD644E") # men, women, orange

ggplot(grid_employed) +
  geom_path(aes(x, y + n * 27, group = interaction(char_idx, stroke, n), size = 1/x_max, color = if_else(x/250 > ratio_w, pal[2],  pal[1])), na.rm = TRUE) +
  scale_size_continuous(range = c(0.5, 1)) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )
