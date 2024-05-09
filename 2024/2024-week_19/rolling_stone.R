library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.5, height = 8, units = "in", dpi = 320)

rolling_stone <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-07/rolling_stone.csv')

## Download album thumbnails
# get_thumbnail <- function(spotify_url) {
#   if(!is.na(spotify_url)) {
#     spotify_oembed = paste0("https://open.spotify.com/oembed?url=", spotify_url)
#     album_json = jsonlite::fromJSON(spotify_oembed)
#     thumbnail = album_json$thumbnail_url
#     download.file(thumbnail, destfile = here::here(paste0("2024/2024-week_19/albums/", spotify_url, ".jpeg")))
#   }
# }
# 
# lapply(rolling_stone$spotify_url, get_thumbnail)


rolling_stone_img <- rolling_stone %>%
  select(album, clean_name, artist_gender, rank_2003:rank_2020, spotify_url) %>% 
  mutate(
    spotify_url = if_else(spotify_url == "6MjOv3BeIjmht2ymtRih3s", "spotify:album:6MjOv3BeIjmht2ymtRih3s", spotify_url),
    thumbnail = here::here(paste0("2024/2024-week_19/albums/", spotify_url, ".jpeg"))
    ) %>% 
  pivot_longer(rank_2003:rank_2020, names_to = "year", values_to = "rank") %>% 
  filter(artist_gender == "Female" | artist_gender == "Male") %>% 
  filter(!is.na(rank)) %>% 
  group_by(artist_gender, year) %>% 
  arrange(rank) %>% 
  mutate(
    x = (row_number() - 1) %% 15,
    y = (row_number() - 1) %/% 15 + 1
  ) %>% 
  add_count() %>% 
  mutate(
    y = if_else(artist_gender == "Male", -y, y),
    xt = last(x),
    yt = last(y),
    year = str_remove(year, "rank_")
    ) %>% 
  ungroup()


f1 <- "Inclusive Sans"
f2 <- "Source Serif Pro"

bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Tam")[5:8]))

ggplot(rolling_stone_img) +
  ggpath::geom_from_path(aes(x, y, path = thumbnail), width = 0.05) +
  geom_tile(aes(x, y, fill = if_else(rank <= 100, NA, "grey10")), width = 0.9, height = 0.9, color = NA, alpha = 0.75) +
  geom_tile(aes(x, y, color = if_else(rank <= 100, "grey97", "grey10")), width = 0.92, height = 0.92, fill = NA, linewidth = 0.25) +
  geom_text(aes(xt, yt, label = n), stat = "unique", family = f1, color = "pink", hjust = 0, nudge_x = 0.7, size = 3.5) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_cartesian(clip = "off", expand = FALSE) +
  facet_wrap(vars(year)) +
  labs(
    title = "The gender gap in Rolling Stone's 500 Greatest Songs of All Time",
    subtitle = "Albums by either female (top) or male artists (bottom) exclusively, with the albums in the top 100 rank highlighted.\nMissing album covers are shown as gray.",
    caption = "Source: Rolling Stone / The Pudding · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_gradient, color = NA),
    panel.spacing.x = unit(2, "lines"),
    strip.text = element_text(color = "grey97", face = "bold", family = f2, size = 15),
    plot.title = element_text(color = "grey97", family = f2, face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey97", margin = margin(4, 0, 20, 0)),
    plot.caption = element_text(color = "grey97"),
    plot.margin = margin(10, 20, 10, 20),
  )
  
