library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') %>% 
  janitor::clean_names()

mus_post <- museums %>% 
  filter(str_detect(year_closed, "9999")) %>% 
  select(museum_id, name_of_museum, postcode, subject_matter) %>% 
  mutate(
    pc_area = str_extract(postcode, "[A-Z][A-Z]*"),
    subjm = case_when(
      str_detect(subject_matter, "-") ~ str_extract(subject_matter, "^.*?-"),
      TRUE ~ subject_matter
    ),
    subjm = str_remove(subjm, "-"),
    subjm = str_replace_all(subjm, "_", " ")
    ) %>% 
  count(pc_area, subjm) %>% 
  group_by(pc_area) %>% 
  slice_max(order_by = n, n = 1) %>% 
  ungroup() %>% 
  add_count(pc_area, name = "ties") %>% 
  mutate(subjm = if_else(ties > 1, "Multiple", subjm))

uk <- read_sf(here::here("2022/2022-week_26/data/postcode_polygons.gpkg"))

uk_mus <- uk %>% 
  left_join(mus_post)

lvls <- sort(unique(mus_post$subjm))
  
f1 <- "Outfit"

pal <- c('#3cb44b', '#e6194B', '#9A6324', '#4363d8', "#469990", "#a9a9a9", '#42d4f4', '#ffd8b1', '#dcbeff', '#800000')

plot_f <- function(df) {
  ggplot(df) +
    geom_sf(aes(fill = subjm), color = "white") +
    scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
    guides(fill = guide_legend(title = "Subject Matter", override.aes = list(color = "grey95"))) +
    coord_sf(clip = "off") +
    theme_void(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "grey95", color = NA)
    )
}

m1 <- plot_f(uk_mus) +
  theme(
    legend.position = c(1.2, 0.6),
    plot.margin = margin(0, 150, 0, 0)
    )

m2 <- plot_f(uk_mus %>% filter(pc_area %in% c("E", "EC", "N", "NW", "SE", "SW", "W", "WC"))) +
  annotate("text", Inf, -Inf, label = "London", hjust = 1, family = f1) +
  theme(legend.position = "none")

m1 +
  inset_element(m2, 1.05, 0, 1.45, 0.5) +
  # plot_layout(widths = c(1.6, 1)) +
  plot_annotation(
    title = "Most common museums by postal code area",
    subtitle = "From about 3 400 museums that have opened since 1621 and are still open",
    caption = "Source: Mapping Museums project · Graphic: Georgios Karamanis"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, margin = margin(0, 0, 10, 0))
  )
  
  
