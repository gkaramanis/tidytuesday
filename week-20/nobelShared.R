library(tidyverse)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

decadesLabels <- function(string) {
  return(as.numeric(string)+1900)
}

filterCategory = "Medicine"

nobel_winners %>%
  filter(category == filterCategory) %>%
  # Uncomment to add "missing" years when filterCategory = Economics:
  # add_row(prize_year = 1901:1968, category = "Economics", prize_share=0/1) %>%
  mutate(nume = as.numeric(str_sub(prize_share, 1, 1)),
         deno = as.numeric(str_sub(prize_share, -1)),
         share = nume/deno,
         year = prize_year %% 10,
         decade = prize_year - 1900 - year) %>%
  group_by(prize_year) %>% 
  distinct(full_name, .keep_all = TRUE) %>% 
  mutate(n = row_number()) %>%
  # Big parts of plot code from https://github.com/spren9er/tidytuesday/blob/master/tidytuesday_201916_new_economist.r
  ggplot() + 
  geom_bar(aes(x = "", y = share, fill = as.factor(n)),
    stat = "identity", show.legend = FALSE
  ) +
  scale_fill_brewer(palette = "Purples") +
  coord_polar("y") +
  facet_grid(decade ~ year, switch = "both",
             labeller = labeller(decade = decadesLabels)) +
  labs(title = paste("Shared Nobel Prizes in ", filterCategory, sep = ""),
       subtitle = "by decade and year, 1901-2016",
       caption = "\nSource: Kaggle | Graphic: Georgios Karamanis / @geokaramanis") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "lightgoldenrod3", colour = "lightgoldenrod3"),
    plot.margin = unit(c(1.6, 0.6, 0.8, 0.8), "cm"),
    text = element_text(family = "IBM Plex Sans", size = 8),
    plot.title = element_text(face = "bold", vjust = 8),
    plot.subtitle = element_text(vjust = 9),
    plot.caption = element_text(size = 4, vjust = -3),
    strip.text.x = element_text(size = 7, 
                                margin = margin(t = 5)),
    strip.text.y = element_text(size = 7, 
      angle = 180, hjust = 1, margin = margin(r = 10))
    ) 

img <- paste("./week-20/nobelShared-", filterCategory, ".png", sep = "")
ggsave(img, height = 5, width = 3.64)
