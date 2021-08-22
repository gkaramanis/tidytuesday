library(tidyverse)
library(gt)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

libraries <- kids %>% 
  filter(variable == "lib") %>% 
  select(state, year, inf_adj_perchild) %>% 
  mutate(inf_adj_perchild = round(inf_adj_perchild * 1000))

plot_group <- function(name, df){
  plot_object <- df %>% 
    mutate(
      start_inf = if_else(year == 1997, inf_adj_perchild, NA_real_),
      end_inf = if_else(year == 2016, inf_adj_perchild, NA_real_)
    ) %>% 
    tidyr::fill(start_inf, end_inf, .direction = "downup") %>% 
    mutate(color = if_else(end_inf - start_inf < 0, "red", "blue")) %>% 
    ggplot(aes(x = year, y = inf_adj_perchild,color = color)) +
    geom_line(size = 15) +
    theme_void() +
    scale_color_identity() +
    theme(legend.position = "none")
  
  return(plot_object)
}

sparklines <- libraries %>% 
  group_by(state) %>% 
  nest() %>% 
  mutate(plot = map2(state, data, plot_group)) %>% 
  select(-data)

table_prepped <- libraries %>% 
  filter(year == 1997 | year == 2016) %>%
  pivot_wider(id_cols = state, names_from = year, values_from = inf_adj_perchild) %>% 
  mutate(change = round((`2016` - `1997` ) / `1997` * 100)) %>%
  inner_join(sparklines, by = "state") %>% 
  mutate(ggplot = NA)

top_55 <- table_prepped %>% 
  filter(dense_rank(change) <= 5 | dense_rank(desc(change)) <= 5) %>% 
  arrange(-change)
  
top_55 %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(ggplot)),
    fn = function(x){
      map(top_55$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>%
  tab_row_group(
    group = "Bottom 5",
    rows = 6:10
  ) %>%
  tab_row_group(
    group = "Top 5",
    rows = 1:5
  ) %>%
  cols_label(
    ggplot = "1997-2016",
    state = "State",
    change = "% Change"
  ) %>%  
  tab_style(
    style = cell_text(font = "Proxima Nova"),
    locations = cells_body(columns = 1)
  ) %>% 
  tab_style(
    style = cell_text(font = "IBM Plex Sans Condensed"),
    locations = cells_body(columns = 2:4)
  ) %>% 
  tab_style(
    style = cell_text(font = "Graphik Compact", weight = "bold"),
    locations = cells_column_labels(columns = gt::everything())
  ) %>% 
  tab_style(
    style = cell_text(font = "Graphik Compact"),
    locations = cells_title()
  ) %>% 
  tab_style(
    style = cell_text(font = "Graphik Compact"),
    locations = cells_row_groups()
  ) %>% 
  tab_header(
    title = "Change in public spending on libraries",
    subtitle = "Dollars spent per child, adjusted for inflation"
  ) %>% 
  tab_source_note("Source: Urban Institute, Table: Georgios Karamanis") %>% 
  tab_options(
    row_group.border.top.width = px(5),
    row_group.border.top.color = "white",
    row_group.border.bottom.width = px(1),
    row_group.border.bottom.color = "lightgrey",
    table.border.top.color = "white",
    table.border.top.width = px(5),
    table.border.bottom.color = "white",
    column_labels.border.bottom.color = "white",
    column_labels.border.bottom.width = px(2),
    column_labels.border.top.width = px(10),
    column_labels.border.top.color = "white"
  ) %>% 
  cols_hide(vars(plot))
