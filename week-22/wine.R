library(tidyverse)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

food <- tribble(
   ~type, ~examples,
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
  select(-c("X1")) %>% 
  distinct() %>% 
  select("variety", "description") %>% 
  filter(str_detect(description, "pair\\w*\\b with")) %>%
  mutate(pairWith = str_extract(description,
                                "pair\\w*\\b with(?:(?=[\\s.?!])[^.?!]*(?:[.?!].*)?)\\.")) %>% 
  mutate(found = str_extract_all(pairWith,
                                 paste(food$examples,collapse="|")))

# at the end
counted <- pairings %>%
  count(variety, sort = TRUE) %>% 
  top_n(5)

# Corvina, Rondinella, Molinara -> Valpolicella
# write.csv(pairings$description, file = "winePairings.csv")
  