library(tidyverse)
library(camcorder)
library(ggpointdensity)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

ggplot(sets %>% filter(year > 1970)) +
  geom_pointdensity(aes(x = year, y = num_parts), size = 7, shape = 15, alpha = 0.5) +
  scale_y_log10() +
  scale_color_viridis_c(option = "turbo") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
  
