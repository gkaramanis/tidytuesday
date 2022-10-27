library(tidyverse)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 14, height = 9, units = "in", dpi = 320)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/ratings.csv')

ratings_series <- ratings %>% 
  group_by(series) %>% 
  summarise(
    v7d_high = max(viewers_7day, na.rm = TRUE),
    v7d_low = min(viewers_7day, na.rm = TRUE),
    # v7d_median = median(viewers_7day, na.rm = TRUE),
    v7d_start = head(viewers_7day, 1, na.rm = TRUE),
    v7d_end = tail(viewers_7day, 1, na.rm = TRUE),
    v7d_arrow = if_else(v7d_end >= v7d_start, 0.01, -0.01),
    # ---
    v28d_high = max(viewers_28day, na.rm = TRUE),
    v28d_low = min(viewers_28day, na.rm = TRUE),
    # v28d_median = median(viewers_28day, na.rm = TRUE),
    v28d_start = head(viewers_28day, 1, na.rm = TRUE),
    v28d_end = tail(viewers_28day, 1, na.rm = TRUE),
    v28d_arrow = if_else(v28d_end >= v28d_start, 0.01, -0.01),
  ) %>% 
  ungroup()

f1 <- "Outfit"
f2 <- "Futura"

col1 <- "#2f77c1"
col2 <- "#ed259d"
col3 <- "#b92f36"
col4 <- "#078A8D"

ggplot(ratings_series) +
  # --- 7 days
  geom_segment(aes(x = series - 0.12, xend = series - 0.12, y = v7d_low, yend = v7d_high), size = 1, color = col1) +
  geom_segment(aes(x = series - 0.08 - 0.12, xend = series + 0.08 - 0.12, y = v7d_start, yend = v7d_start), size = 1, color = col1) +
  geom_segment(aes(x = series - 0.12, xend = series - 0.12, y = v7d_end, yend = v7d_end  + v7d_arrow), arrow = arrow(length = unit(0.02, "npc")), size = 1, color = col1) +
  geom_text(aes(x = series - 0.1 - 0.15, y = v7d_low, label = v7d_low), hjust = 1, family = f1, size = 2.5, check_overlap = TRUE, color = col1) +
  geom_text(aes(x = series - 0.1 - 0.15, y = v7d_high, label = v7d_high), hjust = 1, family = f1, size = 2.5, check_overlap = TRUE, color = col1) +
  geom_text(aes(x = series - 0.1 - 0.15, y = v7d_start, label = v7d_start), hjust = 1, family = f1, size = 2.5, check_overlap = TRUE, color = col1) +
  geom_text(aes(x = series - 0.1 - 0.15, y = v7d_end, label = v7d_end), hjust = 1, family = f1, size = 2.5, check_overlap = TRUE, color = col1) +
  # --- 28 days
  geom_segment(aes(x = series + 0.12, xend = series + 0.12, y = v28d_low, yend = v28d_high), size = 1, color = col2) +
  geom_segment(aes(x = series - 0.08 + 0.12, xend = series + 0.08 + 0.12, y = v28d_start, yend = v28d_start), size = 1, color = col2) +
  geom_segment(aes(x = series + 0.12, xend = series + 0.12, y = v28d_end, yend = v28d_end  + v28d_arrow), arrow = arrow(length = unit(0.02, "npc")), size = 1, color = col2) +
  geom_text(aes(x = series + 0.1 + 0.15, y = v28d_low, label = v28d_low), hjust = 0, family = f1, size = 2.5, check_overlap = TRUE, color = col2) +
  geom_text(aes(x = series + 0.1 + 0.15, y = v28d_high, label = v28d_high), hjust = 0, family = f1, size = 2.5, check_overlap = TRUE, color = col2) +
  geom_text(aes(x = series + 0.1 + 0.15, y = v28d_start, label = v28d_start), hjust = 0, family = f1, size = 2.5, check_overlap = TRUE, color = col2) +
  geom_text(aes(x = series + 0.1 + 0.15, y = v28d_end, label = v28d_end), hjust = 0, family = f1, size = 2.5, check_overlap = TRUE, color = col2) +
  # --- Legend
  geom_segment(data = NULL, aes(x = 1.5 - 0.12, xend = 1.5 - 0.12, y = 10.5, yend = 12.5), size = 1, color = col4) +
  geom_segment(data = NULL, aes(x = 1.5 - 0.08 - 0.12, xend = 1.5 + 0.08 - 0.12, y = 11, yend = 11), size = 1, color = col4) +
  geom_segment(data = NULL, aes(x = 1.5 - 0.12, xend = 1.5 - 0.12, y = 12, yend = 12 + v7d_arrow), arrow = arrow(length = unit(0.02, "npc")), size = 1, color = col4) +
  geom_text(data = NULL, aes(x = 1.5 - 0.1 - 0.15, y = 10.5, label = "Lowest viewership"), hjust = 1, family = f1, size = 3, check_overlap = TRUE, color = col4) +
  geom_text(data = NULL, aes(x = 1.5 - 0.1 - 0.15, y = 12.5, label = "Highest viewership\nof the series"), hjust = 1, family = f1, size = 3, check_overlap = TRUE, lineheight = 0.9, color = col4) +
  geom_text(data = NULL, aes(x = 1.5 - 0.1 - 0.15, y = 11, label = "Series premiere"), hjust = 1, family = f1, size = 3, check_overlap = TRUE, color = col4) +
  geom_text(data = NULL, aes(x = 1.5 - 0.1 - 0.15, y = 11.9, label = "Series finale"), hjust = 1, family = f1, size = 3, check_overlap = TRUE, color = col4) +
  annotate("text", x = 1.5, y = 13.3, label = "How to read", family = f2, size = 6, hjust = 1, color = col4) +
  # --- x axis labels
  annotate("point", x = 1:10, y = 0, size = 10, color = col4) +
  annotate("text", x = 1:10, y = 0, label = 1:10, family = f1, fontface = "bold", size = 6, color = "#D3EFEA") +
  # ---
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  labs(
    x = "Series",
    y = "Million Viewers",
    title = "The Great British<br><span style='font-size:50px'>**Bake Off**</span>",
    subtitle = "Million viewers by series, in the <span style='color:#2f77c1'>**first 7 days**</span><br>and the <span style='color:#ed259d'>**first 28 days**</span> after airtime",
    caption = "Source: bakeoff R package · Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#D3EFEA", color = NA),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(color = col4, size = 15, face = "bold", margin = margin(-5, 0, 0, 0)),
    plot.title = element_markdown(size = 20, margin = margin(0, 0, -60, 0), color = col3),
    plot.subtitle = element_markdown(margin = margin(55, 0, -90, 0), size = 16, lineheight = 1.1),
    plot.caption = element_text(color = col3),
    plot.margin = margin(20, 20, 10, 20)
  )
  
