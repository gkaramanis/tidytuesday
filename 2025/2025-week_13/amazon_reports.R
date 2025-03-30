library(tidyverse)
library(geomtextpath)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

report_words_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-25/report_words_clean.csv')

numbers <- report_words_clean %>% 
  mutate(
    thousand = str_detect(word, "thousand"),
    million = str_detect(word, "million"),
    billion = str_detect(word, "billion"),
    trillion = str_detect(word, "trillion")
    ) %>% 
  group_by(year) %>% 
  summarise(
    n_thousand = sum(thousand, na.rm = TRUE),
    n_million = sum(million, na.rm = TRUE),
    n_billion = sum(billion, na.rm = TRUE),
    n_trillion = sum(trillion, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("n_"), names_to = "type", values_to = "n") %>% 
  mutate(
    type = str_remove(type, "n_"),
    type = fct_inorder(type)
    )
  
annotations <- tribble(
  ~x, ~y, ~label,
  2020, 340, "'Billion' is mentioned more than 'million' for the first time in the 2020 report",
  2020, 4, "That same year 'trillion' appears twice in its first and only report so far",
  )

f1 <- "Montagu Slab 16pt"
f2 <- "Inclusive Sans"

ggplot() +
  ggsankey::geom_sankey_bump(data = numbers, aes(x = year, value = n, fill = type, color = after_scale(colorspace::darken(fill, 0.3)), node = type), smooth = 8, type = "alluvial", space = 0, alpha = 0.9) +
  # Annotations
  ggrepel::geom_label_repel(data = annotations, aes(x, y, label = str_wrap(label, 28)), color = "grey20", min.segment.length = 0, nudge_y = 80, family = f2, size = 5, lineheight = 0.9, label.padding = 0.7, label.r = 0.8, label.size = 0.2, fill = alpha("white", 0.7)) +
  # Scales, theme, etc.
  scale_x_continuous(breaks = seq(2005, 2023, 1), labels = function(x) {return(ifelse(x %% 2 == 0, "·", x))}) +
  scale_y_continuous(breaks = seq(100, 400, 100)) +
  colorspace::scale_fill_discrete_sequential("Oranges") +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_family = f1) +
  labs(
    title = "Amazon’s reports: ‘Billion’ is winning the numbers game",
    subtitle = str_wrap("Total mentions of ‘thousand’, ‘million’, ‘billion’, and ‘trillion’ (including their plural forms) in Amazon’s annual reports.", 90),
    caption = "Source: Amazon annual reports · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme(
    legend.position = c(0.13, 0.35),
    legend.text = element_text(size = 17),
    plot.background = element_rect(fill = "#F0F2F4", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#D9E0E6", size = 0.5),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16, family = f2, margin = margin(3, 0, 30, 0), lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 25, 10, 10)
  )
