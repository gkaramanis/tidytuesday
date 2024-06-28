library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

lgbtq_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-25/lgbtq_movies.csv')

# https://www.themoviedb.org/talk/5daf6eb0ae36680011d7e6ee
genres <- tribble(
  ~genre, ~code,
  "Action", 28,
  "Adventure", 12,
  "Animation", 16,
  "Comedy", 35,
  "Crime", 80,
  "Documentary", 99,
  "Drama", 18,
  "Family", 10751,
  "Fantasy", 14,
  "History", 36,
  "Horror", 27,
  "Music", 10402,
  "Mystery", 9648,
  "Romance", 10749,
  "Science Fiction", 878,
  "TV Movie", 10770,
  "Thriller", 53,
  "War", 10752,
  "Western", 37
)

# Colors not used in plot
keywords_pal <- tribble(
  ~keyword,       ~hex,
  'lgbt',         '#FF0000',  # Red from the rainbow flag
  'gay',          '#FF69B4',  # Pink, associated with gay pride
  'lesbian',      '#D60270',  # Dark pink, from the lesbian pride flag
  'transgender',  '#55CDFC',  # Light blue, from the transgender pride flag
  'bisexual',     '#0033CC',  # Blue, from the bisexual pride flag
  'intersex',     '#FFD700',  # Yellow, from the intersex pride flag
  'queer',        '#6A0DAD',  # Purple, commonly associated with queer
  'genderqueer',  '#B57EDC',  # Lavender, from the genderqueer pride flag
  'non-binary',   '#FCF75E',  # Yellow, from the non-binary pride flag
  'gender',       '#808080',  # Grey, representing non-specific gender
  'asexual',      '#A4A4A4'   # Grey, from the asexual pride flag
)

# Score by genre and keyword
lgbtq_heat <- lgbtq_movies %>% 
  filter(vote_count > 0) %>% 
  mutate(code = str_remove_all(genre_ids, "\\[|\\]")) %>% 
  mutate(code = str_split(code, ", ")) %>% 
  unnest(code) %>%
  mutate(code = as.numeric(code)) %>% 
  filter(!is.na(code)) %>% 
  left_join(genres) %>% 
  mutate(keyword = str_extract_all(overview, paste(keywords_pal$keyword, collapse = "|")) ) %>% 
  unnest(keyword) %>% 
  filter(!is.na(keyword)) %>% 
  group_by(genre, keyword) %>% 
  summarise(score = round(mean(vote_average), 1), n = n()) %>%
  ungroup() %>% 
  group_by(genre) %>% 
  mutate(score_genre = round(mean(score), 1)) %>% 
  ungroup() %>% 
  group_by(keyword) %>% 
  mutate(score_keyword = round(mean(score), 1)) %>% 
  ungroup() %>% 
  mutate(
    # genre = paste0("**", str_replace(genre, " ", "<br>"), "**<br>", score_genre),
    genre = case_when(
      str_detect(genre, "Action") ~ paste0("**", genre, "**<br>avg: ", score_genre),
      TRUE ~ paste0("**", str_replace(genre, " ", "<br>"), "**<br>", score_genre)
    ),
    # keyword = paste0("**", str_to_sentence(keyword), "**<br>", score_keyword),
    keyword = case_when(
      str_detect(keyword, "asexual") ~ paste0("**", str_to_sentence(keyword), "**<br>avg: ", score_keyword),
      TRUE ~ paste0("**", str_to_sentence(keyword), "**<br>", score_keyword)
    )
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("Hiroshige", direction = 1)

ggplot(lgbtq_heat, aes(genre, keyword, fill = score, label = score)) +
  geom_tile(fill = NA, color = "black") +
  geom_point(aes(size = n, color = after_scale(colorspace::darken(fill, 0.5))), alpha = 0.7, shape = 21, stroke = 1) +
  shadowtext::geom_shadowtext(nudge_y = -0.3, family = f1b, color = "black", bg.color = "white") +
  scale_y_discrete(limits = rev) +
  scale_fill_stepsn(colors = pal, breaks = 2:10) +
  # MetBrewer::scale_fill_met_c("Hiroshige", direction = 1) +
  scale_size_continuous(range = c(2, 25)) +
  coord_cartesian() +
  labs(
    title = "LGBTQ+ movie ratings",
    subtitle = str_wrap("A heatmap of average TMDB user ratings for LGBTQ+ movies, showing average ratings by genre and keywords in movie descriptions. Each movie can have multiple keywords. Numbers below genres and keywords indicate their respective average ratings. Circle size represents the number of movies.", 160),
    caption = "Source: TidyRainbow · Graphic: Georgios Karamanis",
    fill = "Avg. rating"
  ) +
  guides(size = FALSE) +
  theme_minimal(base_family = f1b) +
  theme(
    legend.position = "top",
    legend.key.widt lh = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title = element_text(vjust = 1),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = ggtext::element_markdown(lineheight = 1.1),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", family = f2, size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(3, 0, 10, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
