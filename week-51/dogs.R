library(tidyverse)
library(lubridate)

# data
# dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
# dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

dog_colors <- dog_descriptions %>% 
  select(id, posted, size, color_primary) %>% 
  separate(posted, into = "date", sep = "T") %>% 
  mutate(
    year = year(as.Date(date)),
    month = month(as.Date(date))
    ) %>% 
  filter(!is.na(color_primary)) %>% 
  filter(!is.na(year)) %>% 
  group_by(year, size, color_primary) %>% 
  # add_count(color_primary) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() 


ggplot(dog_colors) +
  geom_point(aes(year, color_primary, size = n, color = color_primary)) +
  scale_size(range = c(0, 20)) +
  facet_wrap(vars(size), scales = "free_y")

ggsave(
      here::here("week-51", "plots", "temp", paste0("dogs", ".png")), dpi = 320
      )


# mutate(
#   hex_primary = case_when(
#     color_primary == "Yellow / Tan / Blond / Fawn" ~ "#e5aa70",
#     color_primary == "Apricot / Beige" ~ "#FBCEB1",
#     color_primary == "Brindle" ~ "#af580f",
#     TRUE ~ ""
#   )
# ) %>% 