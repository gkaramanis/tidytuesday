library(tidyverse)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

uniform_colors <- tribble(
  ~pal, ~colors,
  "baylor", c("#244635", "#F5BA47"),
  "louisiana_tech", c("#7AB2E2", "#EA3323"),
  "maryland", c("#E21833", "#ffd200"),
  "north_carolina", c("#86AED0"),
  "notre_dame", c("#11233E", "#398245"),
  "old_dominion", c("#0C2B5C"),
  "purdue", c("#FEFAD2", "#2C2A29", "#000000"),
  "south_carolina", c("#691111", "#000000"),
  "stanford", c("#81221C"),
  "tennessee", c("#EF8833"),
  "texas", c("#B25D22"),
  "texas_a_m", c("#490905"),
  "texas_tech", c("#BB261A", "#000000"),
  "uconn", c("#020E2D", "#7E878D"),
  "southern_california", c("#F7CD46", "#393743", "#8C1A11")
)

tournament_champions <- tournament %>% 
  group_by(school) %>% 
  filter(any(tourney_finish == "Champ")) %>% 
  ungroup() %>% 
  mutate(pal = str_replace_all(tolower(school), " |&", "_")) %>% 
  left_join(uniform_colors)