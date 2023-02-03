library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 9.5, units = "in", dpi = 320)

cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')

cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

center_cats <- cats_uk %>% 
  left_join(cats_uk_reference) %>% 
  arrange(tag_id, timestamp) %>% 
  group_by(tag_id) %>% 
  mutate(
    x0 = ifelse(row_number() == 1, location_long, NA),
    y0 = ifelse(row_number() == 1, location_lat, NA)
  ) %>% 
  fill(x0) %>% 
  fill(y0) %>% 
  mutate(
    x = location_long - x0,
    y = location_lat - y0
    ) %>% 
  ungroup() %>% 
  filter(visible)  %>% 
  filter(!is.na(age_years)) %>% 
  arrange(age_years) %>% 
  mutate(
    life_stage = case_when(
      age_years == 0 ~ "Kitten (<1 year)",
      between(age_years, 1, 6) ~ "Young adult (1-6 years)",
      between(age_years, 7, 10) ~ "Mature adult (7-10 years)",
      age_years > 10 ~ "Senior (>10 years)"
    ),
    life_stage = fct_inorder(life_stage)
  )
  
circles <- data.frame(x0 = 0, y0 = 0, d = seq(2, 0, -0.5)) %>% 
  mutate(
    r = d / 111.32,
    i = rev(row_number())
    )

counts <- center_cats %>% 
  distinct(life_stage, tag_id) %>% 
  count(life_stage)

f1 <- "Outfit"
f2 <- "Lobster"

pal <- rev(MetBrewer::met.brewer("Hokusai2")[3:7])
           
ggplot(center_cats) +
  ggforce::geom_circle(data = circles, aes(x0 = x0, y0 = y0, r = r, fill = r), linewidth = 0.1, color = NA, alpha = 0.5) +
  ggshadow::geom_shadowpath(aes(x, y, group = tag_id), size = 0.5, color = "#F5C400", shadowsize = 0.8, shadowcolor = "#3A3F77") +
  geom_text(data = counts, aes(0, -0.0155, label = paste0(n, " cats")), stat = "unique", family = f1, color = "white") +
  geom_text(data = circles %>%
              filter(i == 3 | i == 5) %>%
              mutate(life_stage = factor("Kitten (<1 year)")),
            aes(x = r * cos(pi/4), y = r * sin(pi/4), label = paste(d, "km")), family = f1, color = pal[2], nudge_x = 0.0011, nudge_y = 0.0013, size = 3) +
  scale_fill_gradientn(colors = pal) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(life_stage), strip.position = "bottom") +
  theme_void(base_family = f1) +
  labs(
    title = "Pet cats home range by life stage",
    subtitle = "Movement patterns of 100 cats in the UK, drawn from a common starting point.\nMost cats were observed over a week. Distances are approximate.",
    caption = str_wrap("Source: McDonald JL, Cole H (2020) Data from: The small home ranges and large local ecological impacts of pet cats [United Kingdom]. Movebank Data Repository. https://doi.org/10.5441/001/1.pf315732 · Graphic: Georgios Karamanis", width = 120)
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#E7E5CC", color = NA),
    strip.text = element_text(size = 20, family = f2, color = "#3A3F77"),
    plot.title = element_text(size = 30, family = f2, color = "#3A3F77", hjust = 0.5, margin = margin(15, 0, 5, 0)),
    plot.subtitle = element_text(color = "#3A3F77", margin = margin(0, 0, 20, 0), hjust = 0.5, size = 13),
    plot.caption = element_text(hjust = 0.5, color = alpha("#3A3F77", 0.7), margin = margin(20, 0, 10, 0))
  )
  
