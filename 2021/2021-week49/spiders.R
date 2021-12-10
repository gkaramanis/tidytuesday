library(tidyverse)
library(ggforce)
library(ggtext)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 11, units = "in", dpi = 320)

spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

spiders_fam <- spiders %>% 
  group_by(family) %>% 
  mutate(
    min_year = min(year),
    n_total = n()
    ) %>% 
  ungroup() %>% 
  mutate(family = fct_reorder(family, min_year)) %>% 
  count(family, n_total, min_year, year) %>% 
  mutate(
    a_deg = as.numeric(family) * 2.7 + 8.5,
    a = a_deg * pi/180,
    x = -(year - min(year) + 10) * cos(a + pi/2.07),
    y = (year - min(year) + 10) * sin(a + pi/2.07),
    label_a = ifelse(a_deg > 180, 270 - a_deg, 90 - a_deg),
    h = ifelse(a_deg > 180, 1, 0),
    label = ifelse(h == 0,
                   paste0(family, " <span style = 'color:darkorange;'>(", n_total, ")</span>"),
                   paste0(" <span style = 'color:darkorange;'>(", n_total, ")</span>", family))
  )

years <- data.frame(
  r = seq(1760 - min(spiders_fam$year) + 10,
          2020 - min(spiders_fam$year) + 10,
          20),
  l = seq(1760, 2020, 20)
  ) %>% 
  mutate(
    lt = ifelse(row_number() %% 2 == 0, "dotted", "solid")
  )


f1 = "Porpora"

ggplot(spiders_fam) +
  # Year circles
  geom_circle(data = years, aes(x0 = 0, y0 = 0, r = r, linetype = lt), size = 0.08, color = "grey50") +
  # Year labels
  geom_label(data = years, aes(x = 0, y = r, label = l), size = 3, family = f1, label.padding = unit(0.25, "lines"), label.size = NA, fill = "grey95", color = "grey70") +
  # Purple points
  geom_point(aes(x = x, y = y, size = n * 10), shape = 21, stroke = 0.15, fill = "purple") +
  # Orange points (totals)
  geom_point(aes(x = -290 * cos(a + pi/2.07), y = 290 * sin(a + pi/2.07), size = n_total), stat = "unique", shape = 21, stroke = 0.5, fill = "orange") +
  # Family names and totals
  geom_richtext(aes(x = -305 * cos(a + pi/2.07),
                y = 305 * sin(a + pi/2.07),
                label = label,
                angle = label_a,
                hjust = h), stat = "unique", family = f1, size = 3.5,
                fill = NA, label.color = NA, color = "#0b5029") +
  # Annotations
  annotate("text", 0, 293, label = "Total", family = f1, color = "orange") +
  scale_size_continuous(range = c(0, 8)) +
  scale_color_viridis_c(option = "turbo") +
  coord_fixed(clip = "off", xlim = c(-400, 400)) +
  labs(
    caption = "<span style = 'font-size:30px;'>Taxonomic publications, 1757-2021</span><br>
    Publications by family and year <span style = 'color:purple;'>(purple circles)</span>and total publications by family <span style = 'color:darkorange;'>(orange circles and text)</span><br>
     <span style = 'color:black;'>Source: World Spider Catalog - Graphic: Georgios Karamanis</span>"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(0, 20, 20, 20),
    plot.caption = element_markdown(family = f1, hjust = 0.5, margin = margin(100, 0, -100, 0), size = 14, lineheight = 1.4, color = "#0b5029")
  )

