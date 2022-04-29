library(tidyverse)
library(camcorder)
library(MetBrewer)
library(ggdist)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

# Users.csv is downloaded from https://www.kaggle.com/datasets/kaggle/meta-kaggle and is not in the repo!
# Read chunked to read in only users in hidden_gems
users_raw <- read_csv_chunked(here::here("2022/2022-week_17/data/Users.csv"), callback = DataFrameCallback$new(function(x, pos) subset(x, UserName %in% hidden_gems$author_kaggle)))

users <- users_raw %>% 
  janitor::clean_names() %>% 
  mutate(register_date = lubridate::mdy(register_date))

hidden_users <- hidden_gems %>% 
  left_join(users, by = c("author_kaggle" = "user_name")) %>% 
  mutate(
    diff_years = lubridate::time_length(date - register_date, "years"),
    icon = substr(author_kaggle, 1, 1),
    year = lubridate::year(date)
    ) %>% 
  group_by(year) %>% 
  mutate(q75 = quantile(diff_years, 0.95, na.rm = TRUE)) %>% 
  ungroup()

people = c(letters, toupper(letters))

ggplot(hidden_users, aes(x = diff_years, y = year, shape = icon, group = year, color = diff_years >= q75)) +
  geom_dots(family = "WeePeople", side = "both", dotsize = 5) +
  scale_x_continuous(breaks = 0:10, labels = c("", 1:10), sec.axis = dup_axis()) +
  scale_y_reverse() +
  scale_shape_manual(values = people) +
  scale_color_manual(values = c("#213e8f", "#f2583c")) +
  labs(
    title = "Years between Kaggle registration and feature on Hidden Gems",
    subtitle = 'By year featured. Highlighted are the 5% "oldest" notebook authors',
    caption = "Source: Martin Henze/Kaggle · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Victor Mono") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#eae9e6", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", size = 0.1),
    panel.grid.minor.x = element_line(color = "grey85", size = 0.1),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(15, 0, 0, 0))
  )
 