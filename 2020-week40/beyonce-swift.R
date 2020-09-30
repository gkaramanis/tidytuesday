library(tidyverse)
library(gt)

charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv') %>% 
  mutate(chart_position = as.numeric(chart_position))

charts_wide <- charts %>% 
  mutate(
    # clean release dates
    released = as.Date(str_remove_all(released, "\\[.+\\]|\\s+\\(.+\\)"), format = "%B %d, %Y"),
    re_release = as.Date(str_remove_all(re_release, "\\[.+\\]|\\(.+\\)"), format = "%B %d, %Y"),
    
    # replace country codes with flags
    chart = str_replace(chart, "AUS", "🇦🇺"),
    chart = str_replace(chart, "CAN", "🇨🇦"),
    chart = str_replace(chart, "FRA", "🇫🇷"),
    chart = str_replace(chart, "GER", "🇩🇪"),
    chart = str_replace(chart, "IRE", "🇮🇪"),
    chart = str_replace(chart, "JPN", "🇯🇵"),
    chart = str_replace(chart, "NZ", "🇳🇿"),
    chart = str_replace(chart, "SWE", "🇸🇪"),
    chart = str_replace(chart, "UK", "🇬🇧"),
    chart = str_replace(chart, "US", "🇺🇸")
  ) %>% 
  # arrange by release date
  arrange(released) %>% 
  pivot_wider(names_from = chart, values_from = chart_position) %>% 
  mutate(
    # add url to album art
    img = paste0("https://raw.githubusercontent.com/gkaramanis/tidytuesday/master/2020-week40/img/", title, ".jpg"),
    formats = strsplit(as.character(formats), ", |/")
    ) %>% 
    unnest(formats) %>% 
    mutate(
      # rename formats
      formats = tolower(formats),
      formats = case_when(
        str_detect(formats, "karaoke") ~ "karaoke",
        str_detect(formats, "streaming") ~ "streaming",
        str_detect(formats, "download") ~ "download",
        TRUE ~ formats
      ),
      # add url to formats
      formats_icon = paste0("<img src='https://raw.githubusercontent.com/gkaramanis/tidytuesday/master/2020-week40/img/", formats, ".png' style='height:16px'>"),
      # merge some columns
      title_artist = paste0("<span style='color:#344072'>**", title, "**</span>", "<br><span style='color:#8086A0'>", artist, "</span>"),
      released_rerelease = paste0(released, "<br><span style='color:#8086A0'>", re_release, "</span>")
      ) %>% 
  distinct(title, formats, .keep_all = TRUE) %>% 
  group_by(title) %>% 
  mutate(all_formats = paste0(formats_icon, collapse = " ")) %>% 
  ungroup() %>% 
  distinct(title, all_formats, .keep_all = TRUE) %>% 
  select(img, title_artist, released_rerelease, all_formats, "🇺🇸":"🇬🇧")

formats_list <- charts %>% 
  mutate(
    formats = strsplit(as.character(formats), ", |/")
  ) %>% 
  unnest(formats) %>% 
  mutate(
    # rename formats
    formats = tolower(formats),
    formats = case_when(
      str_detect(formats, "karaoke") ~ "karaoke",
      str_detect(formats, "streaming") ~ "streaming",
      str_detect(formats, "download") ~ "download",
      TRUE ~ formats
    ),
    # add url to formats
    formats_icon = paste0("<img src='https://raw.githubusercontent.com/gkaramanis/tidytuesday/master/2020-week40/img/", formats, ".png' style='height:16px'>")
  ) %>% 
  distinct(formats, formats_icon) %>% 
  mutate(formats_list = paste(formats_icon, toupper(formats), collapse = " ")) %>% 
  distinct(formats_list) %>% 
  pull()
  

charts_wide %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(img)),
    fn = function(x){
      web_image(url = x, height = 80)
    }
  ) %>% 
  tab_style(
    style = cell_text(font = "Gill Sans", weight = "normal"),
    locations = cells_column_labels(columns = gt::everything())
  ) %>% 
  fmt_markdown(columns = c("title_artist", "released_rerelease", "all_formats")) %>%
  text_transform(
    locations = cells_body(columns = "released_rerelease"),
    fn = function(x) {
      str_replace(x, "NA", "<br>")
    }
  ) %>% 
  text_transform(
    locations = cells_body(columns = gt::everything()),
    fn = function(x) {
      str_replace(x, "NA", "–")
    }
  ) %>% 
  cols_width(
    vars(title_artist) ~ px(200),
    vars(released_rerelease) ~ px(140)
  ) %>%
  tab_spanner(
    label = "Highest chart position",
    columns = vars("🇦🇺", "🇨🇦", "🇫🇷", "🇩🇪", "🇮🇪", "🇯🇵", "🇳🇿", "🇸🇪", "🇬🇧", "🇺🇸")
  ) %>% 
  tab_style(
    style = cell_text(font = "Graphik", weight = "normal"),
    locations = cells_column_spanners(spanners = "Highest chart position")
  ) %>% 
  cols_align(
    columns = vars("all_formats", "🇦🇺", "🇨🇦", "🇫🇷", "🇩🇪", "🇮🇪", "🇯🇵", "🇳🇿", "🇸🇪", "🇬🇧", "🇺🇸"),
    align = "center"
  ) %>% 
  cols_label(
    img = "",
    title_artist = html("Album<br><span style='color:#8086A0'>Artist</span>"),
    released_rerelease = html("Release date<br><span style='color:#8086A0'>re-release</span>"),
    all_formats = "Formats"
  ) %>%  
  tab_source_note(html("Source: Billboard, Table: Georgios Karamanis, Icons (modified): Font Awesome, CC BY 4.0 License<br>", formats_list)) %>% 
  tab_options(
    column_labels.text_transform = "uppercase",
    table.font.color = "#344072"
  )
