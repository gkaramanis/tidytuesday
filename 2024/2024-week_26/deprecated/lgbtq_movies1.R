library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

lgbtq_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-25/lgbtq_movies.csv')

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


lgbtq_movies %>% 
  mutate(code = str_remove_all(genre_ids, "\\[|\\]")) %>% 
  mutate(code = str_split(code, ", ")) %>% 
  unnest(code) %>%
  mutate(code = as.numeric(code)) %>% 
  filter(!is.na(code)) %>% 
  left_join(genres) %>% 
  count(genre, sort = TRUE)

# keywords <- c('lgbt', 'gay', 'lesbian','transgender','bisexual','intersex','queer','genderqueer','non-binary','gender', 'asexual')


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


lgbtq_keywords <- lgbtq_movies %>% 
  mutate(keyword = str_extract_all(overview, paste(keywords_pal$keyword, collapse = "|")) ) %>% 
  unnest(keyword) %>% 
  count(keyword) %>% 
  filter(!is.na(keyword)) %>% 
  left_join(keywords_pal) %>% 
  mutate(keyword = fct_reorder(keyword, n))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(lgbtq_keywords, aes(n, keyword, fill = hex)) +
  geom_col(aes(x = max(n), fill = colorspace::lighten(hex, 0.92))) +
  # bars
  geom_col() +
  # keywords
  geom_text(aes(label = keyword, x = 0), hjust = 1, nudge_x = -10, family = f1b) +
  # n
  geom_text(aes(label = n), hjust = 0, nudge_x = 10, family = f1b) +
  coord_radial(start = -pi/2.1, end = pi/2.1, expand = FALSE, rotate.angle = TRUE, inner.radius = 0.3) +
  scale_fill_identity() +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
