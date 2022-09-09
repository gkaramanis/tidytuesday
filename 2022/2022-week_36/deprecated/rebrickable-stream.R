library(tidyverse)
library(camcorder)
library(ggstream)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')

cols <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')

sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')

inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')

themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

# tree_themes <- themes %>% 
#   left_join(themes, by = c("parent_id" = "id")) %>% 
#   left_join(themes, by = c("parent_id.y" = "id")) %>% 
#   select(id, theme3 = name.x, theme2 = name.y, theme1 = name) %>% 
#   rowwise() %>% 
#   mutate(
#     theme2 = if_else(is.na(theme2), theme3, theme2),
#     theme1 = if_else(is.na(theme1), theme2, theme1)
#   )

cols_yrs <- inventories %>% 
  left_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  left_join(cols, by = c("color_id" = "id")) %>% 
  left_join(inventories, by = "set_num") %>% 
  left_join(sets, by = "set_num") %>% 
  # left_join(tree_themes, by = c("theme_id" = "id")) %>% 
  mutate(hex = case_when(
    # is_trans ~ paste0("#", rgb, "50"),
    !is.na(rgb) ~ paste0("#", rgb),
    TRUE ~ rgb
  )
  ) 

cols_freq <- cols_yrs %>% 
  count(year, hex) %>% 
  filter(year > 1969) %>%
  filter(!is.na(hex)) %>% 
  group_by(year) %>% 
  mutate(
    total = sum(n),
    freq = n / sum(n)
    ) 
  # ungroup() %>% 
  # rowwise() %>% 
  # mutate(
  #   h = DescTools::ColToHsv(hex)[1],
  #   s = DescTools::ColToHsv(hex)[2],
  #   v = DescTools::ColToHsv(hex)[3]
  # ) %>% 
  # ungroup() %>% 
  # arrange(h, s, -v) %>% 
  # mutate(hex = fct_inorder(hex))

# Stacked bars
ggplot(cols_freq, aes(year, freq, group = hex, fill = hex, color = if_else(hex == "#05131D",  "grey70", "grey30"))) +
  geom_tile(position = "fill", stat = "identity", width = 1,  size = 0.1) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_minimal() +
  theme(
    # legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )


# post reddit
ggplot(cols_freq) +
  geom_stream(aes(year, n, fill = hex), color = "grey20", size = 0.1, type = "proportional") +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )


# Top color by year
cols_freq %>% 
  group_by(year) %>% 
  slice_max(order_by = freq, n = 1) %>% 
  ungroup() %>% 
  mutate(
    y = year %/% 10,
    x = year %% 10
  ) %>% 
  ggplot(aes(x = x, y = y, fill = hex, width = 0.95, height = 0.95, label = year)) +
  geom_tile() +
  geom_text() +
  scale_y_reverse() +
  coord_fixed()

library(treemapify)

(p <- cols_freq %>% 
  group_by(year) %>% 
  slice_max(order_by = freq, n = 5) %>% 
  ungroup() %>% 
  ggplot() +
  geom_treemap(aes(area = freq, fill = freq)) +
  # scale_fill_identity() +
  coord_fixed() +
  facet_wrap(vars(year), ncol = 10) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  ))
  
rayshader::plot_gg(p, multicore = TRUE)
  
