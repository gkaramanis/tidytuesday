library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8.2, height = 8.5, units = "in", dpi = 320)

conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')

conf2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

conf2023_lengths <- conf2023 %>% 
  select(session_title) %>% 
  mutate(
    year = 2023,
    title_length = str_length(session_title),
    talk_title = fct_reorder(session_title, title_length)
    )
  
conf2024_lengths <- conf2024 %>% 
  select(talk_title) %>% 
  mutate(
    year = 2024,
    title_length = str_length(talk_title),
    talk_title = fct_reorder(talk_title, title_length)
    ) 

pal <- MetBrewer::met.brewer("Redon")

conf_lengths <- bind_rows(conf2023_lengths, conf2024_lengths) %>% 
  distinct() %>% 
  group_by(year) %>%
  arrange(title_length, .by_group = TRUE) %>%
  mutate(
    y = if_else(year == 2023, row_number() + 2, n():1 - 2),
    hjust = if_else(year == 2023, 0, 1),
    # label = case_when(
    #   year == 2023 ~ paste(title_length, talk_title),
    #   year == 2024 ~ paste(talk_title, title_length)
    # )
    ) %>% 
  ungroup() %>%
  mutate(color = case_when(
    title_length == max(title_length) ~ pal[6],
    title_length == min(title_length) ~ pal[6],
    year == 2023 ~ pal[10],
    year == 2024 ~ pal[4]
    )
  )

f1 <- "Iosevka Fixed"
f2 <- "Publico Text"

p <- ggplot() +
  geom_text(data = conf_lengths, aes(x = if_else(year == 2023, 0, 1), y, label = talk_title, hjust = hjust, color = color), family = f1, size = 2) +
  geom_text(data = conf_lengths, aes(x = if_else(year == 2023, 0, 1), y, label = paste0(" ", title_length, " "), hjust = 1 - hjust, color = color), family = f1, size = 2) +
  scale_color_identity() +
  labs(
    title = "Length of talk titles at posit::conf",
    subtitle = "posit::conf(2024) had both the shortest and the longest title",
    caption = "Source: Posit · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "#9BC4C6", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(5, 0, -10, 0)),
    plot.caption = element_text(hjust = 0.5, size = 8, margin = margin(-10, 0, 10, 0)),
    plot.margin = margin(10, 0, 0, 0)
  )

d <- ggplot(conf_lengths) +
  geom_density(aes(x = title_length, fill = factor(year)), alpha = 0.5, linewidth = 0.4) +
  annotate("text", x = 20, y = 0.015, label = "2023", size = 5, hjust = 1, fontface = "bold", family = f1, alpha = 1, color = pal[10]) +
  annotate("text", x = 85, y = 0.015, label = "2024", size = 5, hjust = 0, fontface = "bold", family = f1, alpha = 1, color = pal[4]) +
  scale_fill_manual(values = c(pal[9], pal[3])) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6)
    )

p + inset_element(d, 0.4, 0.45, 0.6, 0.6) &
  theme(
    plot.background = element_rect(fill = "#9BC4C6", color = NA)
  )

