library(tidyverse)
library(ggridges)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

many_penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-14/many_penguins.csv') |> 
  janitor::clean_names()

mp <- many_penguins |> 
  pivot_longer(beak_length_culmen:tail_length, names_to = "measurement") |> 
  janitor::clean_names() |> 
  mutate(
    sex = sex |> 
      replace_values(
        "F" ~ "Female",
        "M" ~ "Male",
        "U" ~ "Unkn."
      ),
    measurement = measurement |>
      replace_values(
        "beak_length_culmen" ~ "Beak length (culmen)",
        "beak_length_nares" ~ "Beak length (nares)",
        "beak_width" ~ "Beak width",
        "beak_depth" ~ "Beak depth",
        "tarsus_length" ~ "Tarsus length",
        "wing_length" ~ "Wing length",
        "kipps_distance" ~ "Kipp's distance",
        "secondary1" ~ "First secondary length",
        "hand_wing_index" ~ "Hand-wing index",
        "tail_length" ~ "Tail length"
  ),
  value = value / 10
  )

f1 <- "Sofia Sans Extra Condensed"

pal <- rev(wesanderson::wes_palette("FantasticFox1"))

ggplot(mp) +
  geom_density_ridges(aes(x = value, y = sex, fill = sex), rel_min_height = 0.005, alpha = 0.7, scale = 0.9, color = NA) +
  geom_point(aes(x = value, y = sex, color = sex), shape = '▲', size = 1, position = position_nudge(y = -0.1), alpha = 0.5) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = colorspace::darken(pal, 0.3)) +
  scale_x_continuous(labels = scales::label_number(drop0trailing = TRUE)) +
  coord_cartesian(clip = "off") +
  facet_grid(vars(genus), vars(str_wrap(measurement, 15)), scales = "free_x") +
  labs(
    title = "Measuring penguins",
    subtitle = "Ten body measurements (in cm) for 93 penguins from 18 species, across six genera. Each triangle is one penguin, while curves show the distribution by sex.",
    caption = "Data: AVONET database · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(color = c(pal[1], pal[2], pal[3]), face = "bold"),
    strip.text = element_text(size = 12),
    strip.text.x = element_text(face = "bold", vjust = 0),
    strip.text.y = element_text(angle = 0, hjust = 0, face = "bold", size = 14),
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )
