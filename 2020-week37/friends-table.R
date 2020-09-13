library(tidyverse)
library(gt)

friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

friends_table <- friends_info %>% 
  select(season, episode, imdb_rating) %>% 
  mutate(episode = paste0("E", str_pad(episode, 2, "left", "0"))) %>% 
  pivot_wider(names_from = episode, values_from = imdb_rating) %>% 
  mutate(season = paste0("S", str_pad(season, 2, "left", "0")))
  
gt(friends_table) %>% 
  tab_header(
    title = "FRIENDS",
    subtitle = "IMDB Ratings"
    ) %>% 
  cols_label(
    season = ""
  ) %>%
  fmt_missing(
    columns = 2:last_col(),
    missing_text = ""
  ) %>%
  data_color(
    columns = 2:last_col(),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "darkgreen"),
			na.color = "white",
      domain = c(5.5, 10)
      )
  ) %>% 
  tab_style(
      cell_text(
        font = "IBM Plex Mono",
        align = "right"
        ),
      locations = list(
      cells_body(columns = 2:last_col())
      )
      ) %>% 
  tab_style(
    cell_text(
      font = "Futura",
      weight = "bold"
    ),
    locations = list(
      cells_title(groups = c("title"))
    )
  ) %>%
  tab_style(
      list(
        cell_borders(sides = "all", color = "white", style = "solid", weight = px(1.5))),
        locations = list(
          cells_body()
          )
      ) %>% 
  # column and row labels
  tab_style(
      cell_text(font = "IBM Plex Sans"),
    locations = list(
      cells_column_labels(gt::everything()),
      cells_body(columns = 1)
    )
  ) %>% 
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    column_labels.border.top.style = "hidden",
    column_labels.border.bottom.style ="hidden"
  )

  # gtsave(filename = here::here("/2020-week37/plots/friends-table.png"))
