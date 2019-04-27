library(tidyverse)
library(wesanderson)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# remove NA
tidyA <- tidy_anime %>%
  # set end_date to today for titles still airing and NA as end_date
  mutate(end_date = replace(end_date, which(airing & is.na(end_date)), Sys.Date())) %>% 
  # remove titles with NA end_date, remove movies and music
  filter(!is.na(end_date), type != "Movie", type != "Music") %>% 
  select(animeID, name, type, episodes, start_date, end_date) %>% 
  mutate(diff_days = difftime(end_date, start_date,
                              units = c("days"))) %>% 
  mutate(startDateAsNum = as.numeric(start_date)) %>% 
  distinct(name, .keep_all = TRUE) 

pal <- wes_palette("Zissou1", 10, type = "continuous")

tidyA %>%
  ggplot() +
  geom_vline(xintercept = as.Date("2019-04-30"), alpha = 0.2) +
  geom_rect(aes(xmin = start_date, xmax = end_date,
                ymin = startDateAsNum - 30, ymax = startDateAsNum + 30,
                fill = as.numeric(diff_days), alpha = 0.5)) +
  scale_y_reverse() +
  theme_bw() +
  scale_fill_gradientn(colours = pal) +
  theme(
    panel.background = element_rect(fill = "gray90"),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    # panel.grid = element_blank(),
    # legend.position="none",
    text = element_text(family = "IBM Plex Sans", size = 6)
  ) +
  ggsave("./week 17/anime2.png", dpi = 600, height = 6, width = 4)

