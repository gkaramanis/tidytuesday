library(tidyverse)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

unv_hr <- roll_calls %>% 
  filter(str_detect(tolower(short), "declaration of human rights") | str_detect(tolower(short), "human rights declaration")) %>% 
  mutate(
    y = row_number(),
    descr = str_remove_all(descr, "\\\\")
    )
  
ggplot(unv_hr) +
  geom_text(aes(0, y, label = str_wrap(descr, 140)), hjust = 0, vjust = 0, family = "IBM Plex Mono") +
  xlim(0, 15) +
  theme_void()
