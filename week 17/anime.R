library(tidyverse)
library(wesanderson)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# keep ID, names, genre and rating
tidy_anime <- select(tidy_anime, c(1, 2, 3, genre, rating))
# shorten ratings
tidy_anime$rating <- str_replace(tidy_anime$rating, " - .+", "")

# Order ratings to use in y axis
ratLevels <- factor(unique(tidy_anime$rating))
ratLevels <- factor(ratLevels, levels(ratLevels)[c(1, 3, 4, 6, 5)])

pal <- wes_palette("Zissou1", 10, type = "continuous")

tidy_anime %>%
  drop_na() %>% #Drop NA genre
  filter(rating != "None") %>%  #Drop "None" rating
  ggplot(aes(rating, reorder(genre, desc(genre)))) +
  stat_bin2d(aes(fill = stat(count))) + 
  scale_fill_gradientn(colours = pal) +
  scale_x_discrete(limits = levels(ratLevels), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Anime age ratings by genre",
       fill = "number of titles",
       caption = toupper("Data: MyAnimeList.net | Plot: @geokaramanis")) +
  guides(fill = guide_colourbar(title.position="top",
                                direction = "horizontal",
                                barheight = 0.2,
                                title.hjust = 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title.align = -1,
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray20"),
    panel.grid = element_blank(),
    text = element_text(family = "IBM Plex Sans", size = 6)
  ) +
  ggsave("./week 17/anime.png", dpi = 600, height = 4, width = 4)
