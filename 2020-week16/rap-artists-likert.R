library(tidyverse)
library(janitor)
library(ggimage)
library(ggtext)
library(futurevisions)

# Data downloaded from http://datawrapper.dwcdn.net/kOlSQ/5/

rankings <- read_csv(here::here("2020-week16", "data", "data-kOlSQ.csv")) %>% 
  clean_names() %>% 
  select(title_artist = song_artist, img = spotify_thumb_sm, 3:15) %>% 
  filter(n > 4) %>% 
  mutate(
    rank = row_number(),
    title_artist = str_remove(title_artist, "!\\[\\].+"),
    title_artist = str_replace(title_artist, "\\*\\* ", "\\*\\*"),
    img = ifelse(str_detect(img, "data"), NA, img),
    img = ifelse(str_detect(title, "Shook"), "https://i.scdn.co/image/ab67616d00004851a2203fa0656cede30f879b97", img), # fix image for this track
    title = fct_reorder(title, critic_rating),
    n_1 = n1,
    n_2_5 = n2 + n3 + n4 + n5
    ) %>% 
  pivot_longer(cols = n1:n4, names_to = "votes", values_to = "votes_n")

ggplot(rankings) +
  geom_bar(data = subset(rankings, votes == "n1"), aes(fill = votes, x = votes_n, y = title), orientation = "y", position = "stack", stat = "identity", width = 0.8) +
  geom_bar(data = subset(rankings, votes != "n1"), aes(fill = votes, x = -votes_n, y = title), orientation = "y", position = position_stack(reverse = TRUE), stat = "identity", width = 0.8) +
  geom_image(aes(x = n_1 + 1.5, y = title, image = img), size = 0.029, asp = 1.375) +
  geom_richtext(aes(x = n_1 + 2.5, y = title, label = title_artist), size = 7, family = "IBM Plex Sans", hjust = 0, vjust = 0.5, fill = NA, label.color = NA, lineheight = 1.1) +
  annotate("text",  x = 35, y = 13, label = "BBC Music’s\ngreatest hip-hop\nsongs of all time", hjust = 1, vjust = 0, colour = "black", family = "IBM Plex Serif Bold", size = 18, lineheight = 1) +
  annotate("text",  x = 35, y = 10.9, label = "Distribution of critics' votes\nfor songs with 5 votes or more", hjust = 1, vjust = 0, colour = "black", family = "IBM Plex Sans", size = 12, lineheight = 1) +
  geom_vline(xintercept = 0, color = "white") +
  scale_x_continuous(limits = c(-12, 35)) +
  scale_fill_manual(values = rev(futurevisions("cancri")[1:5]), labels = c("favourite", "2nd favourite", "3rd fav.", "4th fav.", "5th fav."), guide = guide_legend(direction = "horizontal", title = "", title.position = "top", title.hjust = 0.5, label.position = "bottom")) +
  labs(
    caption = "Source: BBC Music/Datawrapper | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.815, 0.45),
    legend.spacing.x = unit(0, "pt"),
    legend.key.width = unit(90, "pt"),
    legend.text = element_text(family = "IBM Plex Sans Bold", size = 10),
    # legend.title = element_text(family = "IBM Plex Serif Bold", size = 15),
    plot.background = element_rect(fill = "#a7a6ba", colour = NA),
    plot.caption = element_text(family = "IBM Plex Sans", size = 12, hjust = 0.96),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020-week16", "plots", "temp", paste0("rap-artists-likert-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 22, height = 16)

