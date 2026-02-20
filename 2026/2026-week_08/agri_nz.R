library(tidyverse)
library(gganimate)
library(scales)
library(ggh4x)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

agri_nz <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-17/dataset.csv')

pop_nz <- gt::countrypops |> 
  filter(country_name == "New Zealand") |> 
  select(year, population)

sheep_pop <- agri_nz |> 
  filter(measure == "Total Sheep") |> 
  rename(year = year_ended_june) |> 
  left_join(pop_nz, by = "year") |> 
  mutate(ratio = value / population)

sheep_pop_long <- sheep_pop |> 
  filter(!is.na(population)) |> 
  select(year, sheep = value, people = population, ratio) |> 
  pivot_longer(cols = c(sheep, people, ratio)) 

f1 <- "Inclusive Sans"
f2 <- "LINE Seed Sans"

systemfonts::register_variant(
  name = "LINE Seed Sans Bold",
  family = "LINE Seed Sans",
  weight = "bold"
)

p <- ggplot(sheep_pop_long) +
  geom_point(aes(x = value, y = 0, color = name), size = 2) +
  ggforce::geom_link(aes(x = value, xend = value, y = 0, yend = 1, color = name, size = after_stat(-index)), n = 500) +
  geom_point(aes(x = value, y = 0), size = 0.8, color = "grey99") +
  coord_radial(start = -pi/2, end = pi/2) +
  scale_size_continuous(range = c(0, 2)) +
  scale_color_manual(values = c(people = "#E69F00", ratio = "#56B4E9", sheep = "#009E73")) +
  scale_y_continuous(breaks = c(0.5, 1), expand = FALSE) +
  facet_wrap(vars(name), scales = "free_x", labeller = labeller(name = str_to_title)) +
  facetted_pos_scales(
    x = list(
      name == "people" ~ scale_x_continuous(labels = label_number(scale_cut = cut_long_scale())),
      name == "ratio" ~ scale_x_continuous(labels = label_number(suffix = "x")),
      name == "sheep" ~ scale_x_continuous(labels = label_number(scale_cut = cut_long_scale()))
    )) +
  labs(
    title = "Sheep and people in New Zealand, 1960-2024",
    subtitle = "{closest_state}",
    caption = "Source: Stats NZ · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 16, family = "LINE Seed Sans Bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, family = f2, hjust = 0.5),
    plot.caption = element_text(size = 7, family = f2, hjust = 0.5, margin = margin(t = 10))
  ) 

anim <- p + 
  transition_states(year, wrap = FALSE) +
  shadow_trail(max_frames = 3, alpha = 0.1)
  
animate(anim, width = 10, height = 4, units = "in", res = 320, renderer = av_renderer(here::here("2026/2026-week_08/plots/agri_nz.mp4")), start_pause = 10, end_pause = 20)

animate(anim, width = 10, height = 4, units = "in", res = 320, renderer = gifski_renderer(here::here("2026/2026-week_08/plots/agri_nz.gif")), start_pause = 10, end_pause = 20)
