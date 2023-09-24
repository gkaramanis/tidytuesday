library(tidyverse)
library(ggstream)
library(ggrepel)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)


cran_20230905 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv')

cran_ggplot <- cran_20230905 %>% 
  mutate(year = year(as.Date(Date))) %>% 
  reframe(
    package = Package,
    ggplot_d = str_detect(Depends, "ggplot"),
    ggplot_s = str_detect(Suggests, "ggplot"),
    year
  ) %>% 
  group_by(year) %>% 
  summarise(
    ggs = sum(ggplot_s, na.rm = TRUE),
    ggd = sum(ggplot_d, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(year <= 2023) %>% 
  pivot_longer(ggs:ggd) 
  
annot <- cran_ggplot %>% 
  filter(year == max(year)) %>% 
  mutate(y = c(-20, 120)) %>% 
  mutate(label = case_when(
    name == "ggd" ~ paste0("**", value, "**<br>packages<br>currently"),
    name == "ggs" ~ paste0("**", value, "**<br>packages"),
  ))

f1 <- "Bricolage Grotesque"

ggplot(cran_ggplot) +
  # ggplot2 release
  geom_label_repel(data = NULL, aes(x = 2007, y = 0, label = "ggplot2\nreleased"), family = f1, stat = "unique", direction = "y", nudge_y = 50, label.padding = 0.3, label.size = NA, fill = "#DED4E8", color = colorspace::darken("#DED4E8", 0.8), lineheight = 0.9) +
  geom_point(data = NULL, aes(x = 2007, y = 0), size = 3, stat = "unique", color = colorspace::darken("#DED4E8", 0.5)) +
  # Stream
  geom_stream(aes(x = year, y = value, fill = name, color = after_scale(colorspace::darken(fill))), size = 0.1) +
  geom_richtext(data = annot, aes(x = year + 0.2, y = y, label = label), family = f1, hjust = 0, lineheight = 0.9, label.size = 0, fill = "#DED4E8") +
  scale_x_continuous(breaks = seq(2007, 2023, 5), minor_breaks = 2007:2023) +
  scale_fill_manual(values = c("#C7395F", "#E8BA40")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Number of packages <span style='color:#C7395F'>depending on</span> or <span style='color:#E8BA40'>suggesting</span> ggplot2",
    subtitle = "Aggregated by year of each package's latest CRAN release as of 5 September 2023",
    caption = "Source: CRAN collaboration graph · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#DED4E8", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = colorspace::darken("#DED4E8", 0.1)),
    panel.grid.minor.x = element_line(color = colorspace::darken("#DED4E8", 0.1)),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12, margin = margin(10, 0, 0, 0)),
    axis.text.y = element_blank(),
    plot.margin = margin(10, 50, 10, 10),
    plot.title = element_markdown(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  )