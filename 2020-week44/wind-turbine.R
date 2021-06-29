library(tidyverse)
library(ggforce)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

w_turb <- wind_turbine %>% 
  group_by(province_territory) %>% 
  mutate(
    rotor_r = rotor_diameter_m / 2,
    province_n = cur_group_id(),
    n = n()
    ) %>%
  select(province_n, n, longitude, latitude, hub_height_m, rotor_r) %>% 
  summarise(across(.cols = c(province_n, n, longitude, latitude, hub_height_m, rotor_r), median)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    blade1 = runif(1, 0, 2 * pi),
    blade2 = blade1 + 0.67 * pi,
    blade3 = blade2 + 0.67 * pi,
    blades_x = list(c(0,
                      rotor_r * cos(blade1),
                      0.98 * rotor_r * cos(blade1 + 0.02),
                      0.3 * rotor_r * cos(blade1 + 0.4),
                      0.2 * rotor_r * cos(blade1 + 0.5),
                      0,
                      rotor_r * cos(blade2),
                      0.98 * rotor_r * cos(blade2 + 0.02),
                      0.3 * rotor_r * cos(blade2 + 0.4),
                      0.2 * rotor_r * cos(blade2 + 0.5),
                      0,
                      rotor_r * cos(blade3),
                      0.98 * rotor_r * cos(blade3 + 0.02),
                      0.3 * rotor_r * cos(blade3 + 0.4),
                      0.2 * rotor_r * cos(blade3 + 0.5),
                      0)),
    blades_y = list(c(hub_height_m,
                      hub_height_m + rotor_r * sin(blade1),
                      hub_height_m + 0.98 * rotor_r * sin(blade1 + 0.02),
                      hub_height_m + 0.3 * rotor_r * sin(blade1 + 0.4),
                      hub_height_m + 0.2 * rotor_r * sin(blade1 + 0.5),
                      hub_height_m,
                      hub_height_m + rotor_r * sin(blade2),
                      hub_height_m + 0.98 * rotor_r * sin(blade2 + 0.02),
                      hub_height_m + 0.3 * rotor_r * sin(blade2 + 0.4),
                      hub_height_m + 0.2 * rotor_r * sin(blade2 + 0.5),
                      hub_height_m,
                      hub_height_m + rotor_r * sin(blade3),
                      hub_height_m + 0.98 * rotor_r * sin(blade3 + 0.02),
                      hub_height_m + 0.3 * rotor_r * sin(blade3 + 0.4),
                      hub_height_m + 0.2 * rotor_r * sin(blade3 + 0.5),
                      hub_height_m))
  ) %>% 
  mutate(
    tower_x = list(c(-hub_height_m / 20,
                     hub_height_m / 20,
                     hub_height_m / 60,
                     -hub_height_m / 60)),
    tower_y = list(c(0,
                     0,
                     hub_height_m,
                     hub_height_m))
  ) %>% 
  ungroup()

w_turb_unnested <- w_turb %>% 
  unnest(c(tower_x, tower_y)) %>% 
  unnest(c(blades_x, blades_y))

pal <- c("#388087", "#6FB3B8", "#BADFE7", "#C2EDCE", "#F6F6F2")
col_sky <- pal[3]
col_grass <- pal[1]
col_strip_text <- pal[5]
col_wind <- pal[2]
col_grid <- pal[1]

scales::show_col(pal)

ggplot(w_turb_unnested) +
# turbine -----------------------------------------------------------------
  geom_polygon(aes(x = tower_x, y = tower_y, group = province_n), color = "grey10", fill = "white", size = 0.2) +
  geom_polygon(aes(x = blades_x, y = blades_y, group = province_n), color = "grey10", fill = "white", size = 0.2) +
  geom_point(aes(x = 0, y = hub_height_m), size = 1, shape = 21, color = "grey10", fill = "white", stroke = 0.2) +
  geom_point(aes(x = 0, y = hub_height_m), size = 0.4, shape = 21, color = "grey10", fill = "white", stroke = 0.1) +
# wind --------------------------------------------------------------------
  # top
  geom_segment(aes(x = rotor_r + 50, y = hub_height_m + 3, xend = rotor_r + 50 + n/15, yend = hub_height_m + 3), stat = "unique", size = 0.75, color = col_wind) +
  geom_arc(data = w_turb, aes(x0 = rotor_r + 50, y0 = hub_height_m + 8 + n/max(w_turb$n) * 5, r = 5 + n/max(w_turb$n) * 5, start = pi, end = 1.5 * pi + n/max(w_turb$n) * pi), size = 0.75, n = 20, color = col_wind) +
  # middle
  geom_segment(aes(x = rotor_r + 25, y = hub_height_m - 5, xend = rotor_r + 25 + n/15, yend = hub_height_m - 5), stat = "unique", size = 0.75, color = col_wind) +
  geom_arc(data = w_turb, aes(x0 = rotor_r + 25, y0 = hub_height_m + n/max(w_turb$n) * 5 - 3, r = 2 + n/max(w_turb$n) * 5, start = pi, end = 1.5 * pi + n/max(w_turb$n) * pi), size = 0.75, n = 20, color = col_wind) +
  # bottom
  geom_segment(aes(x = rotor_r + 50, y = hub_height_m - 13, xend = rotor_r + 50 + n/15, yend = hub_height_m - 13), stat = "unique", size = 0.75, color = col_wind) +
  geom_arc(data = w_turb, aes(x0 = rotor_r + 50, y0 = hub_height_m - 18 - n/max(w_turb$n) * 5, r = 5 + n/max(w_turb$n) * 5, start = 0, end = -(0.5 * pi + n/max(w_turb$n) * pi)), size = 0.75, n = 20, color = col_wind) +
  # annotations
  geom_text(aes(x = rotor_r + 70 + n/15, y = hub_height_m, label = paste0(n, ifelse(province_territory == "Alberta", " turbines", ""))), stat = "unique", hjust = 0, vjust = 0.8, family = "Atkinson Hyperlegible", size = 4, color = "grey5") +
  geom_text(aes(x = -60, y = hub_height_m / 2, label = paste0(hub_height_m, " m")), stat = "unique", hjust = 1, vjust = 0.8, family = "Atkinson Hyperlegible", size = 3, color = "grey5") +
  geom_segment(aes(x = -50, y = hub_height_m, xend = -50, yend = 0), stat = "unique", linetype = "dashed", size = 0.6, color = col_wind) +
  geom_text(aes(x = rotor_r / 2, y = hub_height_m + rotor_r + 40, label = paste0(rotor_r, " m")), stat = "unique", hjust = 0.5, vjust = 1, family = "Atkinson Hyperlegible", size = 3, color = "grey5") +
  geom_segment(aes(x = 0, y = hub_height_m + rotor_r + 15, xend = rotor_r, yend = hub_height_m + rotor_r + 15), stat = "unique", linetype = "dashed", size = 0.6, color = col_wind) +
# thems and stuff ---------------------------------------------------------
  coord_fixed(clip = "off") +
  xlim(-100, 350) +
  ylim(0, 190) +
  facet_wrap(vars(province_territory), strip.position = "bottom", ncol = 3) +
  labs(
    title = "Canadian Wind Turbines",
    subtitle = "Median size and total number of wind turbines by province/territory",
    caption = "Source: Government of Canada | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = col_strip_text, color = NA),
    panel.background = element_rect(fill = col_sky, color = NA),
    strip.background = element_rect(fill = col_grass, color = NA),
    strip.text = element_text(size = 13, color = col_strip_text, margin = margin(7, 0, 7, 10), vjust = 1, hjust = 0, family = "Atkinson Hyperlegible Bold"),
    panel.grid.major.y = element_line(color = col_grid, size = 0.1),
    plot.margin = margin(10, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 30, family = "Atkinson Hyperlegible Bold", color = col_grass),
    plot.subtitle = element_text(hjust = 0.5, size = 16, margin = margin(5, 0, 10, 0), color = "grey20"),
    plot.caption = element_text(color = "grey40")
  ) 

ggsave(here::here("temp", paste0("wind-turbine-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 11)
  
