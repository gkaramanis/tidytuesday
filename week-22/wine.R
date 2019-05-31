library(tidyverse)
library(ggimage)

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
  "desserts",  "desserts",
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
  mutate(varietyNr = group_indices()*2) %>% 
  ungroup() %>% 
  # number type groups
  group_by(type) %>% 
  mutate(typeNr = group_indices()) %>% 
  ungroup() 


# images
bottle <- c("./week-22/img/bottle.png")
  
  
ggplot() +
  
  geom_curve(data=pairings,
             aes(x = varietyNr, y = 10,
                 xend = typeNr, yend = 1,
                 color = varietyNr),
             curvature = -0.1,
             size = 0.1,
             alpha = 0.4) +
  
  geom_image(aes(image = bottle, 
                 x = unique(pairings$varietyNr), 
                 y = 11.6)) +
  
  geom_text(aes(label = unique(pairings$variety),
                x = unique(pairings$varietyNr), y = 10.5),
            size = 2) +
  
  # scale_y_discrete(expand = c(0, 1.2)) +
  
  labs(title = "Wine pairings mentioned in reviews",
       subtitle = "top 5 wine varieties paired with 9 food types") +
  
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


ggsave("./week-22/wine.png", height = 5, width = 4)

# Corvina, Rondinella, Molinara -> Valpolicella
# write.csv(pairings$description, file = "winePairings.csv")
  