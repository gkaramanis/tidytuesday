library(tidyverse)
library(ggimage)
library(ggforce)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

food <- tribble(
   ~type, ~example,
  "meat &\ncured meat",    "salami",
  "pasta &\npizza",     "pasta",
  "pasta &\npizza",     "spaghetti",
  "pasta &\npizza",     "lasagna",
  "pasta &\npizza",     "ravioli",
  "vegetables &\nmushrooms",  "mushroom",
  "meat &\ncured meat",      "meat",
  "meat &\ncured meat",      "sausage",
  "meat &\ncured meat",      "beef",
  "meat &\ncured meat",      "hamburger",
  "meat &\ncured meat",      "pork",
  "meat &\ncured meat",      "veal",
  "meat &\ncured meat",      "lamb",
  "meat &\ncured meat",      "ribs",
  "dessert",  "dessert",
  "seafood",   "seafood",
  "seafood",   "scallops",
  "seafood",   "tuna",
  "seafood",   "tilapia",
  "seafood",      "fish",
  "seafood",      "salmon",
  "seafood",    "shrimp",
  "seafood",     "clams",
  "seafood",    "oyster",
  "seafood",   "lobster",
  "cheese",    "cheese",
  "cheese",    "mozzarella",
  "cheese",    "pecorino",
  "poultry",   "poultry",
  "poultry",   "chicken",
  "poultry",   "duck",
  "vegetables &\nmushrooms", "vegetable",
  "vegetables &\nmushrooms", "tomato",
  "vegetables &\nmushrooms", "eggplant",
  "vegetables &\nmushrooms",     "salad",
  "pasta &\npizza", "pizza" 
  )

pairings <- wine_ratings %>%
  # remove X1 and keep unique entries
  select(-c("X1")) %>% 
  distinct() %>% 
  # keep the two variables
  select("variety", "description") %>% 
  # rename Corvina, Rondinella, Molinara to Valpolicella
  mutate(variety = str_replace_all(variety, "Corvina, Rondinella, Molinara", "Valpolicella")) %>% 
  # keep reviews that contain "pair* with" 
  filter(str_detect(description, "pair\\w*\\b with")) %>%
  # keep the varieties with more than X reviews
  group_by(variety) %>% 
  mutate(n = n()) %>%
  filter(n > 48) %>%
  # extract text after "pair* with" and to the end of the sentence
  mutate(pairWith = str_extract(description,
                                "pair\\w*\\b with(?:(?=[\\s.?!])[^.?!]*(?:[.?!].*)?)\\.")) %>% 
  # find matches from the food table
  mutate(found = str_extract_all(pairWith,
                                 paste(food$example,collapse="|"))) %>% 
  # match food
  unnest() %>% 
  left_join(., food, by = c("found" = "example")) %>%
  # number variety groups
  group_by(variety) %>% 
  mutate(varietyNr = group_indices()*2-0.5) %>% 
  ungroup() %>% 
  # number type groups
  group_by(type) %>% 
  mutate(typeNr = group_indices()*1.5-0.5) %>% 
  ungroup() %>% 
  # count pairings per variety and type
  group_by(varietyNr, typeNr) %>% 
  mutate(ntype = n()) %>% 
  ungroup() %>% 
  # needed for images
  rowwise() %>%
  # images and colors
  mutate(foodImg = paste("./week-22/img/", substr(type, 1, 4), ".png", sep = ""),
         bottleColor = ifelse(variety == "Chardonnay" | variety == "White Blend", "#D4C52D", "#5e1224"),
         nudge = runif(1)/5 - 0.1)

# plot pairings
ggplot() +
  # down
  geom_segment(data = pairings,
             aes(x = varietyNr + nudge, y = 10,
                 xend = typeNr + nudge, yend = 1,
                 colour = bottleColor),
             size = pairings$ntype/30,
             lineend = "butt",
             alpha = 0.2) +
  # bottle icons
  geom_image(aes(image = "./week-22/img/bottle.png",
  # color = "#D4C52D",
                 x = c(1.5, 9.5),
                 y = 11.3)) +
  geom_image(aes(image = "./week-22/img/bottle.png",
  # color = "#5e1224",
                 x = c(3.5, 5.5, 7.5),
                 y = 11.3)) +                
  # food icons
  geom_image(aes(image = unique(pairings$foodImg),
                 x = unique(pairings$typeNr),
                 y = 0.3), size = 0.05) +
  # variety and food names
  geom_text(aes(label = unique(pairings$variety),
                x = unique(pairings$varietyNr),
                y = 12.8),
            size = 1.8,
            family = "IBM Plex Serif") +
  geom_text(aes(label = unique(pairings$type),
                x = unique(pairings$typeNr),
                y = -0.4),
            size = 1.4,
            vjust = 1,
            family = "IBM Plex Serif") +

  expand_limits(y = c(1, 13)) +
  scale_color_identity() +

  labs(title = "Wine and food pairings for the top 5 varieties",
       subtitle = "as recommended by WineEnthusiast reviewers",
       caption = "Source: kaggle.com | Graphic: Georgios Karamanis") +
  
  theme_void() +
  theme(
    legend.position =  "",
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = "IBM Plex Serif", size = 8),
    plot.title = element_text(face = "bold", vjust = 8),
    plot.subtitle = element_text(vjust = 9),
    plot.caption = element_text(size = 4, vjust = -3)
  )

ggsave("./week-22/wine.png", height = 5, width = 4, dpi = 600)

# write.csv(pairings$pairWith, file = "winePairings.csv")
  