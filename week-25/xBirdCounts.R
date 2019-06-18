library(here)
library(tidyverse)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

# https://www.massaudubon.org/learn/nature-wildlife/birds/commonly-confused-birds

# Hairy Woodpecker & Downy Woodpecker
# Purple Finch & House Finch
# Chipping Sparrow, American Tree Sparrow, & House Sparrow
# Sharp-Shinned Hawk & Cooper's Hawk

# https://www.aikenaudubon.com/conservation/citizen-science/cbc/frequently-confused-birds/

# American Tree Sparrow and White-crowned Sparrow vs. Chipping Sparrow
# Spotted Towhee vs. Black-headed Grosbeak (male)
# Northern Shoveler (female) vs. Blue-winged Teal (female)

bird_counts %>%
  filter(species == "Hairy Woodpecker" | species == "Downy Woodpecker") %>%
  mutate(how_many_counted = case_when(species == "Downy Woodpecker" ~ -how_many_counted,
    TRUE ~ how_many_counted)) %>%

  ggplot(aes(year, how_many_counted, fill = species)) +
  geom_col() +
  coord_flip() +
  labs(title = "What did I just see?",
       subtitle = "Commonly confused birds",
       caption = "Source: Bird Studies Canada | Graphic: Georgios Karamanis") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) +
  
  # https://datavizproject.com/wp-content/uploads/2016/01/Sk%C3%A6rmbillede-2016-01-28-kl.-16.30.31.png
  
  ggsave(here("week-25", "xBirdCounts.png"))
