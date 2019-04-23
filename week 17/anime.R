library(tidyverse)
library(RColorBrewer)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# keep ID, names, genre and rating
tidy_anime <- select(tidy_anime, c(1, 2, 3, genre, rating))

# Order ratings to use in y axis 
ratLevels <- factor(unique(tidy_anime$rating))
ratLevels <- factor(ratLevels, levels(ratLevels)[c(1, 3, 4, 6, 5)])

tidy_anime %>%
  drop_na() %>% #Drop NA genre
  filter(rating != "None") %>%  #Drop "None" genre
  ggplot(aes(rating, genre)) +
  stat_bin2d(aes(fill = stat(count))) + 
  scale_x_discrete(limits = levels(ratLevels)) +
  theme_minimal() +
  theme(
    )
