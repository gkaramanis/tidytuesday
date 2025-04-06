library(tidyverse)
library(ggridges)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 7, units = "in", dpi = 320)

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

poke_radial <- pokemon_df %>% 
  select(id, type_1, color_1, color_2, hp:speed) %>%
  pivot_longer(hp:speed, names_to = "stat", values_to = "value") %>% 
  group_by(type_1, stat) %>%
  add_count() %>% 
  mutate(type_1_label = paste0("{.", color_1, " █} ", type_1, " (", n, ")")) %>%
  ungroup() %>% 
  mutate(
    stat = case_when(
      stat == "hp" ~ "HP",
      stat == "attack" ~ "Attack",
      stat == "defense" ~ "Defense",
      stat == "special_attack" ~ "Sp. Atk",
      stat == "special_defense" ~ "Sp. Def",
      stat == "speed" ~ "Speed"
    ),
    stat = fct_inorder(stat)
    ) %>% 
  # Normalize
  group_by(stat) %>% 
  mutate(value = value/max(value) * 100) %>% 
  ungroup() %>% 
  group_by(type_1, stat) %>%
  mutate(med_value = median(value)) %>%
  ungroup()
  
f1 <- "Input Mono Narrow"

ggplot(poke_radial, aes(y = stat, x = value, fill = color_1, color = after_scale(colorspace::darken(fill, 0.7)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 2, quantile_lines = TRUE, linewidth = 0.3, jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0, yoffset = -0.25), point_shape = '|', point_size = 2, point_alpha = 1, alpha = 0.7) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20)) +
  facet_wrap(vars(type_1_label), ncol = 6, strip.position = "top") +
  labs(
    title = "Gotta plot 'em all",
    subtitle = "Ridge plots showing normalized stat distributions (0-100) across HP, Attack, Defense, Sp. Attack, Sp. Defense, and Speed",
    caption = "Source: pokemon R package · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing = unit(-0.5, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7, margin = margin(0, 0, 0, 0)),
    panel.spacing.x = unit(1.5, "lines"),
    panel.grid.minor.x = element_blank(),
    strip.text = marquee::element_marquee(margin = margin(15, 0, 0, 0), size = 11),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )
