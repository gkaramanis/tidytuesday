library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')

lgbtq_fiscal <- fiscal_sponsor_directory %>% 
  filter(str_detect(eligibility_criteria, "LGBTQ") | str_detect(project_types, "LGBTQ")) %>% 
  mutate(
    x = year_fiscal_sponsor %% 10,
    y = year_fiscal_sponsor %/% 10 * 10,
    r = sqrt(n_sponsored / pi) / 3.6
  ) %>% 
  group_by(x, y) %>% 
  mutate(
    sponsors_total = n(),
    sponsored_total = sum(n_sponsored),
    label = case_when(
      x == 1 & y == 1980 ~ paste0("**", sponsors_total, " fiscal sponsor** started this year<br>", sponsored_total, " projects have been supported<br>by the fiscal sponsors that started this year"),
      TRUE ~ paste0("**", sponsors_total, "**<br>", sponsored_total)
    )
    ) %>% 
  ungroup()

f1 <- "Founders Grotesk Condensed"

ggplot(lgbtq_fiscal) +
  geom_point(aes(x, y + r, size = n_sponsored), shape = 21, stroke = 0.3, fill = "purple", alpha = 0.2) +
  geom_point(aes(x, y + r, size = n_sponsored), shape = 21, stroke = 0.3, fill = NA, color = "purple3") +
  ggtext::geom_richtext(aes(x, y, label = label), nudge_y = -1.25, stat = "unique", vjust = 1, lineheight = 0.9, family = f1, label.size = 0, fill = NA, size = 4.5) +
  annotate("text", x = 0, y = 1961.8, label = "Fiscal sponsors that support LGBTQ projects", hjust = 0, size = 10, family = f1, fontface = "bold", color = "purple4") +
  annotate("text", x = 0, y = 1956, label = "Number of fiscal sponsors and number of supported projects by the year\nin which each organization started acting as a fiscal sponsor", hjust = 0, size = 7, family = f1, color = "purple4", lineheight = 0.9) +
  annotate("text", x = 0, y = 1948.5, label = "Source: Fiscal Sponsor Directory · Graphic: Georgios Karamanis", hjust = 0, size = 5, family = f1, color = "purple4") +
  annotate("text", x = 8.7, y = 1950, label = "TSNE began operating as a fiscal sponsor in 1959\nand has supported 47 projects since then", hjust = 1, size = 4, family = f1, color = "black", lineheight = 0.9) +
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(1950, 2020, 10)) +
  scale_size_area(max_size = 25) +
  labs(
    x = "year in decade",
    y = "decade"
    ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "green4", linewidth = 0.3)
  )
´  
