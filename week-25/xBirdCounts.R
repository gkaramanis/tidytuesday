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
  mutate(
    pair = case_when(
      species == "Hairy Woodpecker" | 
        species == "Downy Woodpecker" ~ 1,
      species == "American Tree Sparrow" | 
        species == "House Sparrow" ~ 2,
      species == "Sharp-shinned Hawk" | 
        species == "Cooper's Hawk" ~ 3,
                     TRUE ~ 0),
    how_many_counted = case_when(
      species == "Hairy Woodpecker" |
        species == "American Tree Sparrow" |
        species == "Sharp-shinned Hawk"  ~ -how_many_counted,
    TRUE ~ how_many_counted)
    
    ) %>%
  filter(pair != 0) %>% 
  ggplot(aes(year, how_many_counted, fill = species, group = pair)) +
  geom_col() +
  coord_flip() +
  scale_x_reverse() +
  labs(title = "What did I just see?",
       subtitle = "Commonly confused birds",
       caption = "Source: Bird Studies Canada | Graphic: Georgios Karamanis") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#CDB599FF", color = "#CDB599FF"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(family = "IBM Plex Sans")
  ) +
  
  facet_wrap(pair ~., scales = "free_x") +
  
  # https://datavizproject.com/wp-content/uploads/2016/01/Sk%C3%A6rmbillede-2016-01-28-kl.-16.30.31.png
  
  ggsave(here("week-25", "xBirdCounts.png"), height = 7, width = 7)

