library(tidyverse)
library(treemapify)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

isc_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

f1 <- "Outfit"
f2 <- "Sofia Sans Extra Condensed"

pal <- MetBrewer::met.brewer("Johnson", n = length(unique(isc_grants$year))) %>% 
  colorspace::darken(., 0.05)



ggplot(isc_grants, aes(area = funded, fill = factor(year), subgroup = year)) +
  geom_treemap(radius = unit(0.2, "line"), color = "white", size = 2) +
  geom_treemap_text(aes(label = paste0(title, "\n", proposed_by, "\n\n", scales::dollar(funded))), family = f2, reflow = TRUE, grow = TRUE, color = "white") +
  geom_treemap_subgroup_text(aes(label = year), family = f2, color = "white", grow = TRUE, alpha = 0.25) +
  scale_fill_manual(values = rev(pal)) +
  labs(
    title = "Awarded ISC grants by year",
    # subtitle = "Subtitle",
    caption = "Source: R Consortium Infrastructure Steering Committee · Graphic: Georgios Karamanis",
    fill = "Year"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 30, face = "bold", color = "#0D2765", margin = margin(0, 0, 5, 0)),
    plot.caption = element_text(color = "#0D2765", family = f1),
    plot.margin = margin(10, 10, 10, 10)
  )
