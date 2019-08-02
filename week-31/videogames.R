library(tidyverse)
library(here)
library(ggrepel)
library(ggforce)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

overpriced_games <- video_games %>%
  # filter titles over $150 
  filter(price > 150) %>%
  # mark games
  mutate(
    type = case_when(
      grepl("^ADR", game) ~ "game",
      grepl("^Welcome", game) ~ "game",
      grepl("^Tactics", game) ~ "game",
      grepl("^CrisisAction", game) ~ "game",
      grepl("^Bible", game) ~ "game",
      grepl("^Silhouette", game) ~ "game",
      grepl("^安全教育", game) ~ "game",
      T ~ "not_game"
      ),
    # rename games
    game = case_when(
      grepl("安全教育", game) ~ "Safe Education",
      T ~ game
    ),
    # labels
    game_label = case_when(
      grepl("^ADR", game) ~ "The most expensive game on Steam",
      grepl("^Welcome", game) ~ "Deluxe Edition is $7.99",
      grepl("^Tactics 2", game) ~ "Current price is $7.99",
      grepl("^Tactics: Bludgeons", game) ~ "Currently discounted to $16.99",
      # grepl("^CrisisAction", game) ~ "No information found",
      grepl("^Bible", game) ~ "Frequently discounted to $19.99",
      grepl("^Silhouette", game) ~ "Intentionally raised price to halt sales",
      # grepl("^Safe Education", game) ~ "Translated from chinese",
      T ~ ""
    ),
    # not used in the plot:
    game_description = case_when(
      grepl("^ADR", game) ~ "\"Game is designed to give additional\nsupport and an interactive training platform\nfor people studying for their\nADR license.\"",
      grepl("^Welcome", game) ~ "Welcome to Boon Hill Deluxe Edition is\n$7.99",
      grepl("^Tactics 2", game) ~ "\"For russians, the entrance to this page\nis $ 1,000,000,000,000 for 1 second.\"",
      grepl("^Tactics: Bludgeons", game) ~ "By the developer of Tactics 2. \"This\ngame was updated to Tactics 2: War\"",
      grepl("^CrisisAction", game) ~ "It seems that the game has had\nthe same price",
      grepl("^Bible", game) ~ "\"Bible Test is a test for a real\nconnoisseur of this invaluable book.\"",
      grepl("^Silhouette", game) ~ "The reason the game is so expensive\nright now is because I'm trying to put a\nhalt to sales.",
      grepl("^Safe Education", game) ~ "description",
      T ~ ""
    )
    ) %>%
  arrange(., desc(price))

# reorder games, descending price
overpriced_games$game <- reorder(overpriced_games$game, overpriced_games$price)

# dunction to wrap subtitle
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# plot
ggplot(overpriced_games) +
  # bars
  geom_col(aes(game, price, fill = type), width = 0.25) +
  # game title
  geom_text(aes(label = game, x = game, y = 0),
            nudge_x = 0.4, hjust = 0,
            family = "IBM Plex Sans Condensed",
            size = 2, color = "#cecccf") +
  # price
  geom_text(aes(label = price, x = game, y = price),
            nudge_x = 0.4, hjust = 1,
            family = "IBM Plex Sans Condensed",
            size = 2, color = "#cecccf") +
  # annotations
   geom_label_repel(data = subset(overpriced_games, overpriced_games$game_label != ""),  
                     aes(game, price, label = game_label),
                   hjust = 0, force = 10, max.iter = 4000,
                   label.r = 0, ylim = c(320, 600), xlim = c(0, 17),
                   fill = "#627536", color = "#A4CF04", segment.color = "#cecccf", segment.size = 0.1,
                   size = 2.5, family = "IBM Plex Sans Condensed") +
  
  # Didn't use it, looks really nice but too much overlapping :(              
  # geom_mark_circle(aes(game, price, filter = type == "game",
  #                      label = game_label, description = game_description),
  #                  size = 0, expand = 0, label.buffer = unit(100, 'mm'),
  #                  label.fontsize = 8, label.family = "IBM Plex Sans Condensed") +
  
  labs(
    title = "The most expensive games on Steam",
    subtitle = wrapper("There are many games among the most expensive titles, such as design and developer tools, on Steam. Most of the games are of questionable quality and use temporarily increased prices as a way to appear offering generous \"discounts\" when their price gets lowered. The prices in the plot are as captured by Lisa Wood via Steam Spy at the end of July, some comments show current ones.", width = 160),
    caption = "Source: Liza Wood via Steam Spy | Graphic: Georgios Karamanis"
       ) +
  coord_flip(ylim = c(0, 600)) +
  scale_color_manual(values= c("#8aa349", "#2666D3")) +
  scale_fill_manual(values= c("#8aa349", "#2666D3")) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#474b52", color = "#474b52"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Sans Condensed Bold", color = "#cecccf"),
    plot.subtitle = element_text(family = "IBM Plex Sans Condensed", color = "#cecccf", size = 8, margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(family = "IBM Plex Sans", color = "#cecccf", size = 6)
  ) +
  
  ggsave(here("week-31", "img", paste0("videogames", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), width = 8, height = 6, dpi = 320)
  

# https://medium.com/nightingale/the-process-of-familiarity-an-interview-with-nicholas-rougeux-c30f1a1b2f8?source=rss----356ca48206e6---4




