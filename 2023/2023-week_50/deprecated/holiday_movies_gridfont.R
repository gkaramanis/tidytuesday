library(tidyverse)
library(gridfont)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

holiday_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')

grid_holiday <- holiday_movies %>% 
  filter(holiday) %>% 
  select(tconst, primary_title, year, genres) %>% 
  rowwise() %>% 
  mutate(
    h = unlist(gregexpr('Holiday', primary_title))[1],
    g = list(create_text_df(primary_title, font = "smooth"))
    ) %>% 
  ungroup() %>% 
  unnest(g)

grid_h <- grid_holiday %>% 
  filter(h == char_idx & stroke == 1) %>% 
  group_by(tconst) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(tconst, x0 = x)
  
f1 <- "Outfit"

grid_plot <- grid_holiday %>% 
  left_join(grid_h) %>% 
  mutate(
    x2 = x - x0,
    genres_list = as.list(str_split(genres, ","))
  ) %>% 
  unnest(genres_list)
  
ggplot(grid_plot) +
  geom_path(aes(x2, y, group = interaction(tconst, char_idx, stroke), color = genres_list), na.rm = TRUE, linewidth = 0.4, alpha = 0.2) +
  coord_fixed(xlim = c(-240, 260)) +
  facet_wrap(vars(year %/% 10 * 10), ncol = 1) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(margin = margin(10, 0, 5, 0), size = 13, color = "coral3")
  )
  
  

