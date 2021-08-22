library(tidyverse)
library(here)
library(janitor)

jb <- readRDS(here::here("week-53", "data", "jamesbond.RDS")) %>% 
  clean_names() %>% 
  rename(consensual_sex = conquests)

jb %>% 
  mutate(movie = fct_reorder(movie, year)) %>%
  ggplot() +
  geom_col(aes(movie, kills_bond), fill = "red") +
  geom_text(aes(movie, -0.2 + kills_bond, label = kills_bond), hjust = 1, family = "Futura Black", color = "black", size = 7) +
  geom_text(aes(movie, 0.3 + kills_bond, label = toupper(movie)), hjust = 0, family = "Futura Black", color = "white", size = 7.5) +
  scale_y_continuous(limits = c(0, 60), expand = c(0,0)) +
  coord_flip() +
  labs(
    title = toupper("Number of people killed by James Bond"),
    caption = "Data: Handbook of Regression Methods\nGraphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "Futura Medium Italic", size = 30, color = "#CCAD34", hjust = 1, margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(family = "Futura Medium Italic", size = 10, color = "#CCAD34", hjust = 1, margin = margin(10, 0, 0, 0))
  ) 

ggsave(
    here::here("week-53", "plots", "temp", paste0("jb-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10
  )
