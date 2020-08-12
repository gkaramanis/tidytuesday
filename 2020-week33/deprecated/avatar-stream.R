library(tidyverse)
library(fuzzyjoin)
library(ggstream)
library(cartography)

pal1 <- carto.pal("orange.pal", 5, "sand.pal", 2)
pal2 <- carto.pal("green.pal", 2, "pink.pal", 3)
pal3 <- carto.pal("blue.pal", 2)

pal <- c(pal1, pal2, pal3)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

shots <- read_csv(here::here("2020-week33", "data", "shots.csv"))

f <- c("aerial", "back view", "frontal", "ground level", "side view", "cut", "fade", "panning", "zoom", "over-the-shoulder", "overhead", "point-of-view", "close-up", "wide")

scene_shots_joined <- avatar %>% 
  select(id:chapter_num) %>% 
  left_join(scene_description) %>% 
  regex_left_join(shots, by=c("scene_description" = "string"))

scene_shots <- scene_shots_joined %>% 
  distinct(id, shot, .keep_all = TRUE) %>% 
  filter(!is.na(shot)) %>% 
  add_count(book_num, chapter_num, shot)
	
  #mutate(shot = fct_reorder(shot, f))

ggplot(scene_shots) +
  geom_stream(aes(x = chapter_num, y = n, fill = shot), method = "density") +
  facet_grid(vars(type), vars(book_num)) +
  scale_fill_manual(values = pal, breaks = f) +
	theme_minimal() +
  ggsave(paste0("temp/avatar-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), dpi = 320, width = 9, height = 6)
