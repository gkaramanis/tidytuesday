library(tidyverse)
library(skimr)
library(gt)

peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

top_peaks <- peaks %>%
  left_join(expeditions) %>% 
  mutate(decade = floor(year / 10) * 10) %>% 
  # distinct(peak_name, year, decade)
  group_by(peak_name) %>% 
  mutate(
    exp_total = n(),
    hist = inline_hist(decade, n_bins = 12)
    ) %>% 
  ungroup() %>% 
  distinct(peak_name, height_metres, exp_total, hist) %>% 
	slice_max(exp_total, n = 15)

top_peaks %>% 
  gt() %>% 
  data_color(
    columns = vars(height_metres),
    colors = scales::col_numeric(
      palette = c(
        "grey10", "grey30", "grey97"),
      domain = c(min(top_peaks$height_metres), max(top_peaks$height_metres)))
  ) %>% 
  cols_width(
    vars(peak_name) ~ px(150),
    vars(height_metres) ~ px(80),
    vars(exp_total) ~ px(80),
    vars(hist) ~ px(160)
  ) %>%
  cols_align(
    align = "right",
    columns = vars(exp_total)
  ) %>% 
  cols_align(
    align = "right",
    columns = vars(hist)
  ) %>% 
  fmt_number(
    columns = vars(height_metres),
    decimals = 0,
    sep_mark = " "
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
    title = "Himalayan expeditions",
    subtitle = "Top 15 peaks with the most expeditions"
  ) %>% 
  tab_source_note("Source: The Himalayan Database, Table: Georgios Karamanis") %>% 
  cols_label(
    peak_name = "Peak",
    height_metres = "Height (meters)",
    exp_total = "Expeditions (total)",
    hist = "Distribution by decade (1900s - 2010s)"
  ) %>%  
  tab_options(
    table.layout = "auto",
    row_group.border.top.width = px(5),
    row_group.border.top.color = "white",
    row_group.border.bottom.width = px(1),
    row_group.border.bottom.color = "lightgrey",
    table.border.top.color = "white",
    table.border.top.width = px(5),
    table.border.bottom.color = "white",
    column_labels.border.bottom.color = "white",
    column_labels.border.bottom.width = px(1),
    column_labels.border.top.width = px(10),
    column_labels.border.top.color = "white"
  ) 
  # gtsave(here::here("2020-week39", "plots", "himalayan-table.png"), vwidth = 1024, vheight = 768, zoom = 2)
