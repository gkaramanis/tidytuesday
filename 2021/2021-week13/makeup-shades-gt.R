library(tidyverse)
library(gt)

allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')

descrptors <- allCategories %>% 
  filter(str_detect(categories, "descriptor, ")) %>% 
  mutate(name = str_replace(name, "\\/", " ")) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  rowwise() %>% 
  mutate(
    descriptor = str_extract_all(name, "\\w+")[[1]][1],
    after_descriptor = list(str_extract_all(name, "\\w+")[[1]][-1])
    )  %>% 
  ungroup() %>% 
  unnest(after_descriptor) %>% 
  distinct(hex, descriptor, after_descriptor) %>% 
  rowwise() %>% 
  mutate(hex_after_desc = paste0("<span style='padding:0px 4px 0px 4px;border-radius:5px;color:white;background-color:", hex, "'>", after_descriptor, "</span>")) %>% 
  ungroup() %>% 
  group_by(descriptor) %>%
  mutate(
    hex_after_desc = list(sort(unique(hex_after_desc)))
    ) %>%
  ungroup() %>%
  distinct(descriptor, hex_after_desc) %>% 
  rowwise() %>% 
  mutate(hex_after_desc = paste(hex_after_desc, collapse = " ")) %>% 
  ungroup()

descrptors %>% 
  arrange(descriptor) %>% 
  gt() %>% 
  fmt_markdown(columns = everything()) %>% 
  cols_align(
    columns = "hex_after_desc",
    align = "left"
  ) %>% 
  cols_align(
    columns = "descriptor",
    align = "right"
  ) %>% 
  tab_style(
    style = "vertical-align:top",
    locations = cells_body(columns = vars(descriptor))
    ) %>% 
  tab_header(
    title = "Foundation Descriptors",
    subtitle = "Words used to describe the colors of 107 brands and 328 products"
  ) %>% 
  tab_source_note(
    source_note = "Data: The Pudding · Graphic: Georgios Karamanis"
  ) %>% 
  cols_label(
    descriptor = "first",
    hex_after_desc = "second word"
  ) %>% 
  tab_options(
    table.font.names = "American Typewriter",
    heading.title.font.size = 45,
    heading.subtitle.font.size = 22,
    table_body.hlines.width = 0,
    table.border.top.width = 0,
    data_row.padding = px(4),
    table.font.size = "small",
    table.background.color = "#7EE2C0",
    heading.background.color = "#52B394",
    heading.border.bottom.color = "#52B394",
    column_labels.border.bottom.color = "#52B394",
    column_labels.background.color = "#52B394",
    source_notes.background.color = "#52B394",
    table.border.bottom.color = "#52B394",
    table_body.border.bottom.color = "#52B394"
    ) %>% 
  gtsave(here::here("temp", paste0("makeup-shades-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), zoom = 3, vwidth = 600)

