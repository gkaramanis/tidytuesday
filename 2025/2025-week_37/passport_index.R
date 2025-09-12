library(tidyverse)
library(gghighlight)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

# country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')

rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')

visa_free <- rank_by_year |> 
  filter(visa_free_count > 0) |> 
  select(year, visa_free_count)

peak_2006 <- visa_free |> 
  filter(year == 2006) |> 
  summarise(density = list(density(visa_free_count, adjust = 0.5))) |> 
  mutate(
    peak_x = map_dbl(density, ~ .x$x[which.max(.x$y)]),
    peak_y = map_dbl(density, ~ max(.x$y)),
    label = "In 2006, most countries' passports provided access to only a few destinations without a visa"
  )

peak_2025 <- visa_free |> 
  filter(year == 2025) |> 
  summarise(density = list(density(visa_free_count, adjust = 0.5))) |> 
  mutate(
    peak_x = map_dbl(density, ~ {
      idx <- which(.x$x > 150)
      .x$x[idx][which.max(.x$y[idx])]
    }),
    peak_y = map_dbl(density, ~ {
      idx <- which(.x$x > 150)
      max(.x$y[idx])
    }),
    label = "In 2025, significantly more passports give access to most of the world's destinations"
  ) 

col1 <- "#7c2d12"
col2 <- "#1e40af"
col3 <- "#B8860B"

f1 <- "Hoefler Text"
f2 <- "SF Pro Text"

ggplot(visa_free) +
  geom_density(aes(x = visa_free_count, group = year, color = factor(year)), linewidth = 1, adjust = 0.5) +
  gghighlight(year %in% c(2006, 2025), unhighlighted_params = list(linewidth = 0.3, color = "#DDD4C7")) +
  ggrepel::geom_label_repel(data = peak_2006, aes(x = peak_x, y = peak_y, label = str_wrap(label, 50)), nudge_y = 0.0025, direction = "y", hjust = 0, color = col3, family = f1, fontface = "bold", point.padding = 1, segment.size = 0.3, label.size = 0, label.padding = 0.9, size = 4.5, lineheight = 1, fill = NA) +
  ggrepel::geom_label_repel(data = peak_2025, aes(x = peak_x, y = peak_y, label = str_wrap(label, 35)), nudge_y = 0.003, direction = "y", hjust = 1, color = col3, family = f1, fontface = "bold", point.padding = 1, segment.size = 0.3, label.size = 0, label.padding = 1, size = 4.5, lineheight = 1, fill = NA) +
  scale_color_manual(values = c(col1, col2)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "")) +
  labs(
    title = "The world is opening up",
    subtitle = str_wrap("Each country's passport is ranked by the number of destinations (countries and territories) accessible without a visa. The horizontal axis shows visa-free destination counts, while the vertical axis shows how common each access level is (probability density). The curves show how international mobility between nations has expanded from 2006 to 2025.", 120),
    caption = "Source: The Henley Passport Index · Graphic: Georgios Karamanis",
    x = "Number of visa-free destinations",
    y = "Probability density"
  ) +
  theme_minimal(base_size = 14, base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FDF9F3", color = NA),
    plot.title = element_text(face = "bold", size = 22, family = f1, color = "#2C2C2C"),
    plot.subtitle = element_text(size = 12, family = f2, color = "#2C2C2C", lineheight = 1.1),
    plot.caption = element_text(family = f2, color = "#666666", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.y.right = element_text(size = 8, family = f2, color = "#666666"),
    axis.text.x = element_text(family = f2, color = "#2C2C2C"),
    axis.title = element_text(family = f2, color = "#2C2C2C", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  )
  
