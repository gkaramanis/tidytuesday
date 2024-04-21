library(tidyverse)
library(treemapify)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

shiny_revdeps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-16/shiny_revdeps.csv')

shiny_deps <- shiny_revdeps %>% 
  count(dependency_type, parent) %>% 
  filter(dependency_type != "linkingto") %>% 
  filter(n >= 10)

f1 <- "Outfit"
f2 <- "Sofia Sans Extra Condensed"

col1 <- "#EDF2F8"
col2 <- "#5081B9"

ggplot(shiny_deps, aes(area = n, fill = parent == "shiny", label = paste0(parent, "\n", scales::number(n)))) +
  geom_treemap(start = "topleft", layout = "srow", color = "black") +
  geom_treemap_text(start = "topleft", layout = "srow", family = f2) +
  scale_fill_manual(values = c("grey99", "#FFD872")) +
  facet_wrap(vars(dependency_type), nrow = 1) +
  labs(
    title = "Shiny packages dependancies",
    subtitle = "Dependencies of more than 18 000 Shiny packages. Showing packages with dependancies\nfrom at least 10 other packages",
    caption = "Source: CRAN · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col2, color = NA),
    strip.text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(fill = "#F2D4CC"),
    plot.title = element_text(size = 30, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 16, face = "bold", color = col1),
    plot.caption = element_text(color = col1, size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )
