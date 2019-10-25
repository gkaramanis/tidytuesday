# Messy code ahead, proceed at your own risk!

library(tidyverse)
library(here)
library(shadowtext)
library(ggforce)
library(grid)
library(lubridate)
library(cowplot)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

horror_month <- horror_movies %>% 
  mutate(
    release_year = as.numeric(str_extract(title, "(?<=\\()\\d{4}(?=\\))")),
    title = str_remove(title, " \\(.+\\)")
  ) %>% 
  filter(release_year < 2001) %>% 
  mutate(
    date = dmy(release_date),
    month = month(date, label = TRUE, abbr = FALSE),
    cast = lengths(str_split(cast, "\\|")),
    movie_run_time = parse_number(movie_run_time)
    ) %>%
  filter(!is.na(month)) %>%
  filter(!is.na(review_rating)) %>% 
  filter(!is.na(movie_run_time)) %>% 
  add_row(title = "dummy1", release_year = 1982.4) %>%
  add_row(title = "dummy2", release_year = 1982.8) %>%
  add_row(title = "dummy3", release_year = 1985.4) %>%
  add_row(title = "dummy4", release_year = 1985.8) %>% 
  mutate(title = fct_reorder(title, release_year))

# grob needs(?) relative coordinates (0-1)
skull = tribble(
  ~x, ~y,
  0.15, 0.97,
  0.85, 0.97,
  0.85, 0.33,
  0.65, 0.33,
  0.7, 0.1,
  0.3, 0.1,
  0.35, 0.33,
  0.15, 0.33
)

nose = tribble(
  ~x, ~y,
  0, -2.5,
  1, -5,
  -1, -5
)

### Legend ##################
l <- ggplot() +
  # web
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = 8, r = 12, angle = 0), fill = NA, color = "purple4") +
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = 8, r = 10, angle = 0), fill = NA, color = "purple4") +
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = 8, r = 8, angle = 0), fill = NA, color = "purple4") +
  geom_shadowtext(aes(x = 5.5, y = -6, label = "Cast Members\nListed by IMDB"), hjust = 0, color = "darkorchid4", family = "Feast of Flesh BB", size = 7, lineheight = 0.7) +
  # skull with jaw
  annotation_custom(
    xsplineGrob(
      x = skull$x,
      y = skull$y,
      gp = gpar(fill = "grey30"),
      open = FALSE, shape = 0.8
    )) +
  # eyes
  geom_circle(aes(x0 = -4, y0 = 0, r = 3.5), fill = "black", color = "grey20", size = 3) +
  geom_shadowtext(aes(x = -4, y = -0, label = "IMDB\nRating"),  color = "red4", family = "Feast of Flesh BB", size = 8, lineheight = 0.7) +
  geom_circle(aes(x0 = 4, y0 = 0, r = 3.5), fill = "black", color = "grey20", size = 3) +
  geom_shadowtext(aes(x = 4, y = -0, label = "Movie\nRun Time"), color = "red4", family = "Feast of Flesh BB", size = 8, lineheight = 0.7) +
  # nose
  annotate("polygon", x = nose$x, y = nose$y, fill = "black") +
  # teeth
  geom_text(aes(x = 0, y = -8.2, label = "Release\nYear"), color = "black", family = "Feast of Flesh BB", size = 12, lineheight = 0.7) +
  geom_shadowtext(aes(x = 0, y = -12, label = "Title"), color = "white", family = "Feast of Flesh BB", size = 12) +
  # title
  geom_shadowtext(aes(x = 0, y = 8, label = "Horror Movies\nof the 20th Century"), color = "white", family = "Feast of Flesh BB", size = 16, lineheight = 0.8) +
  # theme and stuff
  xlim(c(-12, 12)) +
  ylim(c(-12, 12)) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    strip.text = element_blank(),
    plot.margin = margin(30, 30, 30, 30)
  )
###########################


### Plot ##################
p <- ggplot(horror_month) +
  
  # web
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = cast, r = 12, angle = 0), fill = NA, color = "purple4") +
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = cast, r = 10, angle = 0), fill = NA, color = "purple4") +
  geom_regon(data = subset(horror_month, !is.na(cast)), aes(x0 = 0, y0 = 0, sides = cast, r = 8, angle = 0), fill = NA, color = "purple4") +
  geom_shadowtext(aes(x = 6, y = -6, label = cast), hjust = 0, color = "darkorchid4", family = "Feast of Flesh BB", size = 9) +
  
  
  # skull with jaw
  annotation_custom(
    xsplineGrob(
      x = skull$x,
      y = skull$y,
      gp = gpar(fill = "grey30"),
      open = FALSE, shape = 0.8
    )) +
  
  # eyes
  geom_circle(aes(x0 = -4, y0 = 0, r = review_rating/2), fill = "black", color = "grey20", size = 2) +
  geom_shadowtext(aes(x = -4, y = -0, label = review_rating),  color = "red4", family = "Feast of Flesh BB", size = 8) +
  geom_circle(aes(x0 = 4, y0 = 0, r = movie_run_time/30), fill = "black", color = "grey20", size = 2) +
  geom_shadowtext(aes(x = 4, y = -0, label = paste0(movie_run_time, "'")), color = "red4", family = "Feast of Flesh BB", size = 8) +
  
  # nose
  annotate("polygon", x = nose$x, y = nose$y, fill = "black") +
  
  # teeth
  geom_text(aes(x = 0, y = -8.2, label = release_year), color = "black", family = "Feast of Flesh BB", size = 11) +
  geom_shadowtext(aes(x = 0, y = -12, label = title), color = "white", family = "Feast of Flesh BB", size = 5) +
  
  # theme and stuff
  xlim(c(-12, 12)) +
  ylim(c(-12, 12)) +
  labs(caption = "Source: IMDB via Kaggle | Graphic: Georgios Karamanis") +
  coord_fixed() +
  facet_wrap(vars(title), ncol = 4) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    strip.text = element_blank(),
    plot.margin = margin(20, 20, 10, 20),
    plot.caption = element_text(size = 5, family = "Feast of Flesh BB", color = "grey80")
  )
###########################
p

### Combine  ################## 
ggdraw(p) +
  draw_plot(l, 0.25, 0.25, 0.495, 0.495) +
  ggsave(
    here::here("week-43", "plots", "temp", paste0("horror-movies-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    width = 14, height = 14, dpi = 320
  )

