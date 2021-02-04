library(tidyverse)
library(janitor)
library(ggforce)
library(ggtext)

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  clean_names() %>% 
  mutate(
    female_a = females / total_enrollment
  )

pal <- c("#C53028", "#994D83", "#3F93D5", "#AECB52", "#F7CC47")

ggplot(hbcu_all) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 300 * female_a, b = 300 * female_a * 4, angle = 0, fill = female_a), color = "grey30") +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = total_private/200, r = total_private/180, start = 0.5, end = 2*pi - 0.5), color = "grey30", fill = "grey85") +
  geom_segment(aes(x = total_private/180, y = -200, xend = total_private/180, yend = -2600), linetype = "dotted", color = pal[2]) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = total_public/200, r = total_public/185, start = 0, end = 2*pi), color = "grey30", fill = "grey85") +
  geom_segment(aes(x = total_public/180, y = -200, xend = total_public/180, yend = -1400), linetype = "dotted", color = pal[1]) +
  geom_text(aes(x = 0, y = -2000, label = paste0(round(female_a * 100), "%")), color = "grey30", family = "JetBrains Mono ExtraBold") +
  geom_text(aes(x = total_public/200, y = -2000, label = paste0(round(total_public/1000), "K")), color = pal[1], family = "JetBrains Mono ExtraBold") +
  geom_text(aes(x = total_private/200, y = -3200, label = paste0(round(total_private/1000), "K")), color = pal[2], family = "JetBrains Mono ExtraBold") +
  scale_color_viridis_c(option = "plasma", direction = 1) +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  coord_fixed(ratio = 0.25, clip = "off", ylim = c(-3900, 1500)) +
  facet_wrap(vars(year), ncol = 4) +
  labs(
    title = "Enrolment in <span style = 'color:#C53028'>public</span> and <span style = 'color:#994D83'>private</span> HBCU",
    subtitle = "Total number of students enrolled by year and percentage of female students",
    caption = "Source: HBCU | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Graphik Compact Medium Italic", base_size = 18) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey80", color = NA),
    plot.margin = margin(20, 23.5, 20, 23.5),
    strip.text = element_text(color = "grey50", size = 12),
    plot.title = element_markdown(color = "grey10"),
    plot.subtitle = element_text(family = "Graphik Compact", margin = margin(10, 0, 20, 0), color = "grey15"),
    plot.caption = element_text(family = "Graphik Compact Regular Italic", margin = margin(20, 0, 0, 0))
  ) +
ggsave(here::here("temp", paste0("hbcu-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 10, height = 13)

