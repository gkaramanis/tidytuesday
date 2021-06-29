library(tidyverse)
library(ggforce)
library(prismatic)

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


diversity_calc <- diversity_school %>%
  filter(!is.na(state)) %>%  # filter NA states
  group_by(state, category) %>% 
  mutate(
    enrollment = sum(enrollment),  # calculate total enrollment for every state
    total_enrollment = sum(total_enrollment) 
    ) %>% 
  select(-name) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(
    percentage = round(enrollment / total_enrollment, 4), # calculate percentage per state
    perc_square = percentage ^ 2  # square for diversity index
    ) %>% 
  ungroup() %>% 
  filter(category != "Women" & category != "Total Minority") %>%  # filter out
  group_by(state) %>% 
  mutate(
    di = sum(perc_square),  # calculate diversity index per state
    category_nr = row_number()  # number categories (for ggplot)
    ) %>% 
  ungroup()

f = 0.7  # change to change shape of the "balloon"

diversity_shapes <- diversity_calc %>% 
  rowwise() %>% 
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    x = list(c(0,
               f * percentage * sin(category_nr * 2 * pi / 9 - pi/4),
               percentage * sin(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * percentage * sin(category_nr * 2 * pi / 9 + pi/5),
               0
               )),
    y = list(c(0,
               f * percentage * cos(category_nr * 2 * pi / 9 - pi/5),
               percentage * cos(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * percentage * cos(category_nr * 2 * pi / 9 + pi/4),
               0
               ))
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(state, di), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_9)

# Category colors
pal <- c("#3cb44b",
         "#e6194B",
         "#469990",
         "#4363d8",
         "#800000",
         "#42d4f4",
         "#f032e6",
         "#f58231",
         "#ffe119"
         )

# Pull categories from the dataset
cat <- diversity_school %>% 
  distinct(category) %>% 
  filter(category != "Women" & category != "Total Minority") %>% 
  pull()

# Join colors with categories
pal_df <- data.frame(c = pal, l = cat)

# Filter diversity index to get 12+12 states, reorder from lower to higher di (lower = more diversity)
diversity_flower <- diversity_shapes %>% 
  filter(di > 0.537 | di < 0.31) %>% 
  mutate(state = fct_reorder(state, di))

# Plot
ggplot(diversity_flower) + 
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = state, fill = pal[1]), alpha = 0.7) +
  geom_bspline_closed(aes(x_2, y_2, group = state, fill = pal[2]), alpha = 0.7) +
  geom_bspline_closed(aes(x_3, y_3, group = state, fill = pal[3]), alpha = 0.7) +
  geom_bspline_closed(aes(x_4, y_4, group = state, fill = pal[4]), alpha = 0.7) +
  geom_bspline_closed(aes(x_5, y_5, group = state, fill = pal[5]), alpha = 0.7) +
  geom_bspline_closed(aes(x_6, y_6, group = state, fill = pal[6]), alpha = 0.7) +
  geom_bspline_closed(aes(x_7, y_7, group = state, fill = pal[7]), alpha = 0.7) +
  geom_bspline_closed(aes(x_8, y_8, group = state, fill = pal[8]), alpha = 0.7) +
  geom_bspline_closed(aes(x_9, y_9, group = state, fill = pal[9]), alpha = 0.7) +
  scale_fill_identity(guide = guide_legend(title = "", nrow = 2, override.aes = list(alpha = 0.7, shape = 2)), breaks = pal, labels = pal_df$l) +
  coord_fixed() +
  facet_wrap(vars(state), ncol = 5) +
  labs(
    title = "Student Diversity at More Than 4,600 Institutions",
    subtitle = "Showing the 12 states with the highest and the 12 with the lowest Simpson's diversity index, calculated at state level",
    caption = "Source: Chronicle of Higher Education | Graphic: Georgios Karamanis"
  ) +
  # Theme
  theme_void(base_family = "IBM Plex Sans", base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(family = "IBM Plex Sans Condensed"),
    legend.margin = margin(40, 0, 0, 0),
    plot.background = element_rect(fill = "grey97", colour = NA),
    strip.text.x = element_text(family = "IBM Plex Serif Bold", size = 17, colour = "grey20", margin = margin(0, 0, 10, 0)),
    plot.title = element_text(margin = margin(20, 0, 10, 0), hjust = 0.5, size = 25, family = "IBM Plex Serif Medium"),
    plot.subtitle = element_text(margin = margin(0, 0, 55, 0), hjust = 0.5, size = 18, colour = "grey20"),
    plot.caption = element_text(margin = margin(40, 0, 0, 0), hjust = 0.5, colour = "grey20", family = "IBM Plex Sans Light"),
    plot.margin = margin(20, 20, 35, 20)
  ) 

ggsave(here::here("2020-week11", "plots", "diversity-school2020.png"), dpi = 320, width = 15, height = 13.25)
    
