library(tidyverse)
library(camcorder)
library(ggpointdensity)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')

psych_stats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')

myers_briggs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/myers_briggs.csv')

  
f1 <- "Outfit"
f2 <- "PoliteType"

happysad <- psych_stats %>% 
  filter(question == "happy/sad") %>% 
  mutate(score = if_else(personality == "happy", avg_rating, 100 - avg_rating))

myers_briggs %>% 
  left_join(happysad) %>%
  ggplot(aes(x = score, y = avg_match_perc)) +
  geom_pointdensity(size = 1, shape = 18) +
  geom_smooth(method = "lm", color = "grey30") +
  geom_tile(aes(x = 50, y = 80, width = Inf, height = 8,
                fill = case_when(
                  str_starts(myers_briggs, "INTP") ~ "grey70", 
                  str_starts(myers_briggs, "ENT|EST") ~ "#A2BEFF", 
                  str_starts(myers_briggs, "INT|IST") ~ "coral2", 
                  TRUE ~ "cornflowerblue")
                )) +
  geom_text(aes(x = 50, y = 80, label = myers_briggs), color = "white", stat = "unique", family = f1, fontface = "bold") +
  scale_y_continuous(breaks = seq(30, 70, 10)) +
  scale_color_viridis_c(option = "turbo") +
  scale_fill_identity() +
  facet_wrap(vars(myers_briggs)) +
  labs(
    title = "Not everyone loves a happy character",
    subtitle = "People that self-identify as INTJ, ISTJ and ISTP get lower match scores with characters that have been rated as happier",
    x = "Happy/sad score of character (higher score means happier)",
    y = "Average match percentage"
    ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_blank(),
    axis.text = element_text(color = "grey60", size = 7),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(size = 20, family = f2)
  )
  
