library(tidyverse)
library(gt)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

libraries <- kids %>% 
  filter(variable == "lib") %>% 
  mutate(inf_adj_perchild = round(inf_adj_perchild * 1000)) 

plot_spark <- function(data){
  data %>% 
    ggplot(aes(x = year, y = inf_adj_perchild)) +
    geom_line(size = 15) +
    theme_void()
}

libraries_plot <- libraries %>% 
  select(state, year, inf_adj_perchild) %>% 
  nest(spent = c(year, inf_adj_perchild)) %>% 
  mutate(plot = map(spent, plot_spark))


libraries %>% 
  filter(year == 1997 | year == 2016) %>%
  pivot_wider(id_cols = state, names_from = year, values_from = inf_adj_perchild) %>% 
  mutate(change = round((`2016` - `1997` )/ `1997` * 100)) %>% 
  filter(dense_rank(change) <= 5 | dense_rank(desc(change)) <= 5) %>% 
  arrange(desc(change)) %>% 
  mutate(ggplot = NA) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(ggplot)),
    fn = function(x){
      map(libraries_plot$plot, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>% 
  cols_label(
    ggplot = "1997-2016"
  ) %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
  ) %>% 
  tab_source_note("Source: Urban Institute | Table: Georgios Karamanis")
  