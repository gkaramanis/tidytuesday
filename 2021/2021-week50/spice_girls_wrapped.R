library(tidyverse)
library(camcorder)
library(gridfont)
library(hershey)

gg_record(dir = "temp", device = "png", width = 7, height = 11, units = "in", dpi = 320)

related_artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv')

genres <- related_artists %>% 
  rename(genre = genres) %>% 
  count(genre, sort = TRUE) %>% 
  mutate(
    genre = fct_reorder(genre, n),
    f = n / max(n)
    ) %>% 
  head(5) %>% 
  mutate(i = rev(row_number())) %>% 
  rowwise() %>% 
  mutate(
    genre_df = list(create_string_df(as.character(toupper(genre)), font = "futural"))
  )

grid_genres <- genres %>% 
  unnest(cols = genre_df) %>% 
  group_by(genre) %>% 
  mutate(
    # shift x to right, so that every genre starts at 0
    x = x -(min(x)),
    # calculate "length" of each genre
    l = max(x, na.rm = TRUE),
    # change x and y so that they fit inside the bars and align to them
    x = x * 40 / l * f + 0.7,
    y = y/2.5 + 10 * i + 4.5,
    # for geom_rect
    xmin = 0,
    xmax = n * 2 + 1.3,
    ymin = min(y, na.rm = TRUE),
    ymax = max(y, na.rm = TRUE)
  )

# https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/#checking-that-a-font-is-installed-and-available

fa_solid_path <- systemfonts::system_fonts() %>% 
  filter(name == "Gotham-Medium") %>% 
  pull(path)

systemfonts::register_font(
  name = "Gotham Medium",
  plain = fa_solid_path
)

f1 = "Gotham Medium"
f2 = "Gotham"

c1 = "#22306E"
col_bg = "#E377BA"
col_bars = c("#413149", "#3317D8", "#F6D5D9", "#DAF37E", "#A7ECBB")
col_text = c("#DEAD8F", "#AAEEBC", "#D83436", "#4A3854", "#243369")

ggplot(grid_genres) +
  # Rank text
  geom_text(aes(x = -10, y = 10 * i + 5, label = paste0("#", 6 - i)), size = 14, stat = "unique", family = f2, fontface = "bold", color = c1) +
  # Bars
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = 10 * i + 0.5, ymax = 10 * i + 9.5,
                fill = col_bars[6 - i]), stat = "unique") +
  # Genres inside bars
  geom_path(aes(x, y, group = interaction(genre, char_idx, stroke), color = col_text[6 - i]), na.rm = TRUE, size = 2.5) +
  # Genres outside bars
  geom_text(aes(x = xmax + 4, y = 10 * i + 5, label = str_to_sentence(genre)), hjust = 0, stat = "unique", size = 6, family = f1, color = c1) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_cartesian(clip = "off", xlim = c(-12, 75)) +
  labs(
    title = "Spice Girls Related Artists Top Genres",
    caption = "Based on genre count among 21 related artists\nSource: Spotify · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = f1, color = c1, size = 22),
    plot.caption = element_text(family = f1, color = c1, size = 12, hjust = 0.5)
  )

