# Messy code warning!
# More comments coming soon

library(tidyverse)
library(camcorder)
library(colorspace)
library(patchwork)

gg_record(dir = "temp", device = "png", width = 9, height = 11, units = "in", dpi = 320)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')

race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

pal <- c("#448D88", "#7900f1", "#F9F6F4") # men, women, background

race_d <- race %>% 
  select(race_year_id, distance)

ultra_d <- ultra_rankings %>% 
  left_join(race_d) %>% 
  filter(!is.na(nationality) & !is.na(time) & distance > 0 & !is.na(gender) & age > 0) %>% 
  mutate(nationality = toupper(nationality)) %>% 
  group_by(nationality, gender) %>% 
  summarise(nationality, gender, med_pace = median(time_in_seconds/distance)) %>% 
  ungroup() %>% 
  distinct() %>% 
  pivot_wider(names_from = gender, values_from = med_pace) %>% 
  mutate(
    g_diff = M - W,
    nationality = fct_reorder(nationality, g_diff),
    x = ifelse(g_diff > 0, -10, 10),
    hjust = ifelse(g_diff > 0, 1, 0)
    # color = ifelse(g_diff > 0, pal[1], pal[2])
    ) %>% 
  filter(!is.na(g_diff))

f1 = "Roboto Condensed"
f2 = "Avenir Black"
f3 = "Montserrat Black"

# Title and annotations
annot <- data.frame(
  g_diff = c(360, -360, -360, -360, -360),
  nationality = c("MDA", "CRC", "EST", "HKG", "CHI"),
  label = c("Faster\nWomen", "Faster\nMen", "TRAIL\nRUN-\nNING", "Difference of median pace between\nmen and women by nationality.\n \nCalculated from 16 814 results for\nwomen and 95 275 results for men\nacross 85 nationalities. The number\nof results by nationality are shown\nin the plot on the right.", "Source: International Trail Running Association\nGraphic: Georgios Karamanis"),
  hjust = c(1, 0, 0, 0, 0),
  vjust = c(1, 1, 1, 1, 1),
  color = c(pal[2], pal[1], "grey20", "grey20", "grey30"),
  family = c(f2, f2, f3, f1, f1),
  size = c(20, 20, 26, 5.5, 4)
)

format_hm <- function(sec) str_sub(format(sec), start = -5L)

p1 <- ggplot(ultra_d) +
  geom_segment(aes(x = 0, xend = g_diff, y = nationality, yend = nationality), alpha = 0.9) +
  geom_point(aes(x = g_diff, y = nationality), size = 2, shape = 21, fill = "grey40") +
  geom_text(aes(x = x, y = nationality, label = nationality, hjust = hjust), stat = "unique", family = f1, size = 3) +
  geom_text(data = annot, aes(x = g_diff, y = nationality, label = label, hjust = hjust, vjust = vjust, color = color, size = size, family = family), lineheight = 0.8) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_x_time(labels = format_hm, breaks = seq(-3600, 3600, 60)) +
  coord_cartesian(clip = "off", xlim = c(-340, 340)) +
  labs(x = "Difference in median pace (MM:SS)") +
  theme_minimal(base_family = f2) +
  theme(
    plot.background = element_rect(fill = pal[3], color = NA),
    axis.title.x = element_text(margin = margin(7, 0, 0, 0), color = "grey40"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(margin = margin(10, 0, 0, 0), color = "grey10"),
    axis.text.y = element_blank(),
    strip.text = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Runners by nationality for inset plot
stats <- ultra_rankings %>% 
  left_join(race_d) %>% 
  filter(!is.na(nationality) & !is.na(time) & distance > 0 & !is.na(gender) & age > 0) %>% 
  mutate(nationality = toupper(nationality)) %>% 
  count(nationality, gender) %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  filter(!is.na(W) & !is.na(M)) %>% 
  mutate(nationality = fct_rev(nationality))

# Inset plot
p2 <- ggplot(stats) +
  geom_rect(aes(xmin = 900, xmax = 900 + M, ymin = as.numeric(factor(nationality)) - 0.4, ymax = as.numeric(factor(nationality)) + 0.4), fill = pal[1]) +
  geom_text(aes(x = 900 + M, y = as.numeric(factor(nationality)), label = M), hjust = 0, family = f1, nudge_x = 200, size = 1.5) +
  geom_rect(aes(xmin = -900, xmax = -900 - W, ymin = as.numeric(factor(nationality)) - 0.4, ymax = as.numeric(factor(nationality)) + 0.4), fill = pal[2]) +
  geom_text(aes(x = -900 - W, y = as.numeric(factor(nationality)), label = W), hjust = 1, family = f1, nudge_x = -200, size = 1.5) +
  geom_text(aes(x = 0, y = as.numeric(factor(nationality)), label = nationality), family = f1, fontface = "bold", size = 1.2) +
  coord_cartesian(clip = "off") +
  theme_void()

# Combine plots
p1 +
  inset_element(p2, 0.6, 0, 1, 0.72) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(10, 10, 10, 10)
    )
  )

