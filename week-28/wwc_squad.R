library(tidyverse)
library(here)

squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")

# squads <- squads %>% 
#   mutate(f = ifelse(squad_no %/% 12 == 0, "Starting 11", "Substitutes"))

ggplot(squads) +
  geom_count(aes(factor(pos, level = c("GK", "DF", "MF", "FW")), squad_no), show.legend = F) +
  stat_summary(fun.data = n_fun, geom = "text") +
  scale_size_area(max_size = 9) +
  scale_y_reverse(breaks = c(1:23)) +
  scale_x_discrete(position = "top",
    labels = c("Goalkeepers", "Defenders", "Midfielders", "Forwards")) +
  # facet_wrap(~ f, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.title = element_blank()
  ) +
  ggsave(here("week-28", "wwc_squad.png"),
         width = 5, height = 7, dpi = 300)
