library(tidyverse)
library(ggrepel)

# polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')


ggplot(rankings) +
  geom_text_repel(aes(x = year, y = 0, label = title), direction = "y", size = 3) +
  ggsave(here::here("2020-week16", "plots", "temp", paste0("rap-artists-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 20, height = 20)
