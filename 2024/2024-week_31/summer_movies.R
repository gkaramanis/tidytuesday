library(tidyverse)
library(ggsankey)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 7, units = "in", dpi = 320)

summer_movie_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movie_genres.csv')

summer_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movies.csv')

# Movies by genre and decade
summer_genres <- summer_movies %>% 
  filter(title_type == "movie") %>% 
  select(tconst, primary_title, year, runtime_minutes, average_rating) %>% 
  mutate(decade = factor(year %/% 10 * 10)) %>% 
  left_join(summer_movie_genres) %>% 
  group_by(decade) %>% 
  mutate(decade_n = n()) %>% 
  ungroup() %>% 
  group_by(decade, genres) %>% 
  summarise(
    score = median(average_rating, na.rm = TRUE),
    n = n(),
    decade_n,
    prop = n / decade_n
    ) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(!is.na(decade) & !is.na(genres) & !is.na(score))
  
# Create a bump chart, it will be modified later
p <- ggplot(summer_genres, aes(x = decade, node = genres, fill = genres, value = prop, label = genres)) +
  geom_sankey_bump() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

# Create labels at the starting point for each genre
g_labs_start <- ggplot_build(p) %>% 
  .$data %>% 
  .[[1]] %>% 
  group_by(label) %>% 
  filter(x == min(x)) %>% 
  reframe(
    x,
    y = mean(y)
  ) %>% 
  left_join(summer_genres %>% group_by(genres) %>% filter(as.numeric(decade) == min(as.numeric(decade))), by = c("label" = "genres"))

# Create labels at the ending point for each genre
g_labs_end <- ggplot_build(p) %>% 
  .$data %>% 
  .[[1]] %>% 
  group_by(label) %>% 
  filter(x == max(x)) %>% 
  reframe(
    x,
    y = mean(y)
  ) %>% 
  left_join(summer_genres %>% group_by(genres) %>% filter(as.numeric(decade) == max(as.numeric(decade))), by = c("label" = "genres"))

# Fonts
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

# Colors
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69"
  )

na_col <- "#866f85"

# Final plot
ggplot() +
  # Lines
  geom_sankey_bump(data = summer_genres, aes(x = decade, node = genres, fill = if_else(genres %in% c("Drama", "Comedy", "Romance"), genres, NA), value = prop)) +
  # Left-side labels
  shadowtext::geom_shadowtext(data = g_labs_start, aes(x, y, label = paste(label, "·", n), color = if_else(label %in% c("Drama", "Comedy", "Romance"), label, NA)), hjust = 1, nudge_x = -0.1, family = f1b, bg.color = "grey99", fontface = "bold") +
  # Right-side labels
  shadowtext::geom_shadowtext(data = g_labs_end, aes(x, y, label = paste(label, "·", n), color = if_else(label %in% c("Drama", "Comedy", "Romance"), label, NA)), hjust = 0, nudge_x = 0.1, family = f1b, bg.color = "grey99") +
  scale_fill_manual(values = pal, na.value = na_col) +
  scale_color_manual(values = pal, na.value = na_col) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  labs(
    title = "What 'summer' movies are made of: drama, comedy, and romance",
    subtitle = str_wrap("The chart shows the genre distribution of IMDb-listed movies with 'summer' in their titles, by decade. Only movies with at least 10 votes are included. Each film can have up to three genre tags. Line thickness represents the proportion of genre occurrences within each decade. The numbers at each end of a line show the genre count in the first and last decades it appears.", 140),
    caption = "Source: IMDB · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0),
    plot.margin = margin(10, 40, 10, 20)
  )
