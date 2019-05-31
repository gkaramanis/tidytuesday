library(tidyverse)
library(ggimage)
library(ggforce)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

food <- tribble(
   ~type, ~example,
  "cured meat",    "salami",
  "pasta",     "pasta",
  "pasta",     "spaghetti",
  "pasta",     "lasagna",
  "mushroom",  "mushroom",
  "meat",      "meat",
  "meat",      "sausage",
  "meat",      "beef",
  "meat",      "hamburger",
  "meat",      "pork",
  "meat",      "veal",
  "meat",      "lamb",
  "dessert",  "dessert",
  "seafood",   "seafood",
  "seafood",      "fish",
  "seafood",    "shrimp",
  "seafood",     "clams",
  "seafood",    "oyster",
  "seafood",   "lobster",
  "cheese",    "cheese",
  "cheese",    "mozzarella",
  "poultry",   "poultry",
  "poultry",   "chicken",
  "vegetables", "vegetable",
  "vegetables", "tomato",
  "vegetables", "eggplant",
  "vegetables",     "salad",
  "pizza", "pizza" 
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
  mutate(typeNr = group_indices()) %>% 
  ungroup() %>% 
  # images
  mutate(bottleImg = "./week-22/img/bottle.png",
         foodImg = paste("./week-22/img/", type, ".png", sep = ""),
         bottleColor = ifelse(variety == "Chardonnay" | variety == "White Blend", "#5e1224", "#e3c979"))

  
ggplot() +
  
  geom_diagonal(data=pairings,
             aes(x = varietyNr, y = 10,
                 xend = typeNr, yend = 1,
                 color = varietyNr),
             curvature = -0.1,
             size = 0.2, 
             alpha = 0.4) +
  
  geom_image(aes(image = unique(pairings$bottleImg), 
                 x = unique(pairings$varietyNr), 
                 y = 11.6)) +
  
  geom_image(aes(image = unique(pairings$foodImg), 
                 x = unique(pairings$typeNr), 
                 y = 0.4), size = 0.05) +
  
  geom_text(aes(label = unique(pairings$variety),
                x = unique(pairings$varietyNr), y = 10.4),
            size = 2) +
  
  expand_limits(y = c(1, 13)) +
  scale_color_identity() +
  
  labs(title = "Top 5 wine varieties paired with 9 food types",
       subtitle = "as mentioned by tasters in reviews",
       caption = "Source: XXX | Graphic: Georgios Karamanis") +
  
  theme_void() +
  theme(
    legend.position =  "",
    plot.background = element_rect(fill = "#ebd9a0", colour = "lightgoldenrod3"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(family = "IBM Plex Sans", size = 8),
    plot.title = element_text(face = "bold", vjust = 8),
    plot.subtitle = element_text(vjust = 9),
    plot.caption = element_text(size = 4, vjust = -3)
  )


ggsave("./week-22/wine.png", height = 5, width = 4, dpi = 600)

# write.csv(pairings$description, file = "winePairings.csv")
  