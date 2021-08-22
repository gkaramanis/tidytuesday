library(tidyverse)
library(ggforce)
library(here)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

trees <- sf_trees %>% 
  filter(species != "Tree(s) ::" & species != "::") %>% 
  group_by(species) %>% 
  mutate(med_dbh = median(dbh, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(species, med_dbh) %>% 
  filter(med_dbh > 24.5) %>% 
  mutate(
    r = med_dbh,
    x = lapply(r/2, function(x)
    {y = rnorm(4, 1, r/30)
    x*c(-1, 1, 1, -1) + y}),
    y = lapply(r/2, function(x)
    {y = rnorm(4, 1, r/30)
    x*c(1, 1, -1, -1) + y})
  ) %>%
  unnest(cols = c(x, y)) %>% 
  separate(species, sep = " :: ", into = c("sci_name", "name"))

ggplot(trees) +
  # trunk outline
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = "#DCB484", size = 1.25) +
  
  # diameter line and value
  geom_segment(aes(x = -med_dbh/2, y = -med_dbh/2 - 2,
           xend =  med_dbh/2, yend  = -med_dbh/2 - 2), colour = "grey50") +
  geom_text(aes(x = 0, y = -med_dbh/2 - 7, label = paste0(med_dbh, " in")), family = "JetBrains Mono Medium", size = 2.5, colour = "grey40", check_overlap = TRUE) +
  
  # tree rings
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.8, expand = unit(-3, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.8, expand = unit(-5, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.6, expand = unit(-7, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.8, expand = unit(-9, 'mm')) +  
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.6, expand = unit(-11, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.5, expand = unit(-13, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.5, expand = unit(-15, 'mm')) +
  geom_bspline_closed(aes(x, y, group = name), color = "black", fill = NA, size = 0.5, expand = unit(-17, 'mm')) +
  
  # species labels
  geom_text(aes(x = 0, y = 50, label = name), vjust = "top", family = "IBM Plex Sans Bold", size = 3, color = "grey20") +
  geom_text(aes(x = 0, y = 43, label = sci_name), vjust = "top", family = "IBM Plex Sans Medium", size = 2.8, color = "grey40") +
  
  facet_wrap(vars(sci_name), ncol = 5) +
  coord_fixed(xlim = c(-45, 45)) +
  labs (
    title = "San Francisco tree species with the largest\nmedian diameter at breast height",
    caption = "Source: DataSF | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_blank(),  # element_text(family = "IBM Plex Sans Bold", size = 9, color = "grey20"),
    axis.title.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(20, 26, 20, 26),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 18, margin = margin(0, 0, 20, 0), hjust = 0.5),
    plot.caption = element_text(family = "JetBrains Mono", margin = margin(20, 0, 0, 0), hjust = 0.5)
  ) 

ggsave(here::here("2020-week05", "plots", "sf-tree-trunks.png"), dpi = 320, width = 11, height = 8)

