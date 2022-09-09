library(tidyverse)
library(camcorder)
library(isocuboids)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 12, units = "in", dpi = 320)


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
  # left_join(tree_themes, by = c("theme_id" = "id")) %>% 
  mutate(hex = case_when(
    # is_trans ~ paste0("#", rgb, "50"),
    !is.na(rgb) ~ paste0("#", rgb),
    TRUE ~ rgb
  )
  ) 

theme_sel <- themes %>% 
  filter(str_detect(name, "Star Wars"))

cols_freq <- cols_yrs %>% 
  filter(theme_id %in% theme_sel$id) %>% 
  count(theme_id, year, hex) %>% 
  filter(year > 1969) %>%
  filter(!is.na(hex)) %>% 
  group_by(year) %>% 
  mutate(
    total = sum(n),
    freq = n / sum(n)
  ) 

cf <- cols_freq %>% 
  left_join(themes, by = c("theme_id" = "id")) %>% 
  select(-parent_id) %>% 
  rename(theme_name = name) %>% 
  group_by(theme_name, year) %>% 
  slice_max(order_by = freq, n = 2, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    y = year %/% 10,
    x = year %% 10
  ) 

list_of_df <- cf %>% 
  group_by(theme_id) %>% 
  group_split()

freq_fill <- cf %>% 
  select(theme_id, freq, hex, year, theme_name) 

matrix_from_df <- function(df) {
  df %>%
    select(x, y, freq) %>%
    xtabs(freq~x+y, .) %>%
    DescTools::as.matrix.xtabs() %>%
    cuboid_matrix(., return_data = TRUE) %>%
    left_join(freq_fill, by = c("y" = "freq")) %>%
    group_by(cuboid_id) %>%
    mutate(
      lx = mean(px),
      ly = mean(py)
    ) %>%
    ungroup()
}

combined_df <- lapply(list_of_df, matrix_from_df) %>% 
  bind_rows()

f1 <- "Outfit"

ggplot(combined_df %>% filter(!is.na(theme_name)))+
  geom_polygon(aes(px, py, group = plot_group, fill = hex), col = "grey97") +
  geom_text(aes(lx, ly, label = year), nudge_y = 0.2, stat = "unique", color = "white", family = f1, size = 2) +
  scale_fill_identity() +
  facet_wrap(vars(theme_name)) +
  coord_fixed() +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
