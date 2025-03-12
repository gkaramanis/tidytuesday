library(tidyverse)
library(ggtext)
library(ggimage)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

# Read in TidyTuesday data
pixar_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv')

public_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv')

# Read IMDB ratings
pixar_imdb <- read_csv(here::here("2025/2025-week_11/data/pixar_imdb.csv"))

# Find films with the biggest discrepancies between critics and audiences
pixar_discr <- public_response %>%
  left_join(pixar_films %>% select(film, release_date), by = "film") %>% 
  mutate(film = str_replace(film, "-", "·")) %>%
  left_join(pixar_imdb, by = "film") %>%
  mutate(
    cinema_score_num = case_when(
      cinema_score == "A+" ~ 10,
      cinema_score == "A" ~ 9,
      cinema_score == "A-" ~ 8,
      TRUE ~ NA_real_
    ),
    critic_score = (rotten_tomatoes + metacritic) / 20,
    audience_score = (cinema_score_num + imdb_rating) / 2,
    discrepancy = critic_score - audience_score,
    film_year = paste0("**", film, "** (", year(release_date), ")")
  ) %>%
  filter(!is.na(discrepancy)) %>% 
  mutate(film_year = fct_reorder(film_year, discrepancy))

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Bricolage Grotesque 12pt Condensed"

pal <- c("#6A3D9A", "#FFAC33") 

ggplot(pixar_discr) +
  # Lines
  geom_segment(aes(x = film_year, xend = film_year, y = audience_score, yend = critic_score, color = discrepancy > 0), linewidth = 1.5) +
  # Points and icons
  geom_point(aes(x = film_year, y = audience_score, size = if_else(discrepancy < 0, 10, 7)), fill = pal[1], shape = 21, color = "purple4", stroke = 1) +
  geom_point(aes(x = film_year, y = critic_score, size = if_else(discrepancy > 0, 10, 7)), fill = pal[2], shape = 21, color = "gold",  stroke = 1) +
  geom_text(aes(x = film_year, y = audience_score, label = "🍿", size = if_else(discrepancy < 0, 5, 3))) +
  geom_text(aes(x = film_year, y = critic_score, label = "🎥", size = if_else(discrepancy > 0, 4.5, 3))) +
  # Film name
  geom_richtext(data = . %>% filter(discrepancy > 0 & audience_score < 10), aes(x = film_year, y = audience_score - 0.12, label = film_year), family = f1, hjust = 1, size = 6, label.color = NA, label.padding = unit(0, "lines"), fill = NA, vjust = 0.6) +
  geom_richtext(data = . %>% filter(discrepancy < 0 & audience_score == 10), aes(x = film_year, y = critic_score - 0.12, label = film_year), family = f1, hjust = 1, size = 6, label.color = NA, label.padding = unit(0, "lines"), fill = NA, vjust = 0.6) +
  geom_richtext(data = . %>% filter(discrepancy < 0 & audience_score != 10), aes(x = film_year, y = audience_score + 0.14, label = film_year), family = f1, hjust = 0, size = 6, label.color = NA, label.padding = unit(0, "lines"), fill = NA, vjust = 0.6) +
  # Add subtitle in plot
  geom_textbox(aes(x = 18.6, y = 4.4, label = "**Comparing critic scores** 🎥 <span style='font-size:24px'>(average of Rotten Tomatoes and Metacritic on 10-point scale)</span><br>**with audience ratings** 🍿 <span style='font-size:24px'>(average of CinemaScore converted to 10-point scale: A+ = 10, A = 9, A- = 8, and IMDB rating)</span><br>**Films ordered by the difference between critic and audience scores**"), family = f2, size = 8, hjust = 0, vjust = 1, lineheight = 1.05, width = unit(19, "lines"), stat = "unique", box.padding = unit(0.8, "lines"), box.color = NA, fill = alpha("grey99", 0.7)) +
  # Logo
  geom_image(aes(x = 20.5, y = 5.48), image = here::here("2025/2025-week_11/img/pixar_logo.png"), size = 0.46, stat = "unique") +
  coord_flip() +
  scale_y_continuous(limits = c(4.4, 10), breaks = 4:10) +
  scale_color_manual(values = c("TRUE" = pal[2], "FALSE" = pal[1])) +
  scale_size_identity() +
  labs(
    caption = "Source: {pixarfilms} & IMDB · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#eaf0e7", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid = element_line(color = alpha("darkgreen", 0.15)),
    axis.text = element_text(size = 18),
    plot.caption = element_text(size = 11, family = f2, hjust = 0.05, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 0, 10, 0)
  )