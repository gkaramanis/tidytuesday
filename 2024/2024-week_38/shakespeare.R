library(tidyverse)
library(tidytext)
library(zoo)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

romeo_juliet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/romeo_juliet.csv')

top_characters <- romeo_juliet %>% 
  count(character, sort = TRUE) %>% 
  slice_max(order_by = n, n = 3)

rj_sentiment_char <- romeo_juliet %>%
  unnest_tokens(output = word, input = dialogue) %>% 
  left_join(get_sentiments("bing")) %>% 
  filter(!is.na(sentiment) & !is.na(line_number)) %>% 
  count(line_number, character, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(total = positive - negative) %>% 
  group_by(character) %>%
  arrange(character, line_number) %>%
  mutate(rm = rollmean(total, 20, fill = NA, align = "right")) %>%
  ungroup() %>% 
  filter(character %in% c("Romeo", "Juliet"))
  
neg_lines <- rj_sentiment_char %>% 
  group_by(character) %>% 
  slice_min(order_by = total, n = 2) %>% 
  ungroup() %>% 
  left_join(romeo_juliet)

pos_lines <- rj_sentiment_char %>% 
  group_by(character) %>% 
  slice_max(order_by = total, n = 2) %>% 
  ungroup() %>% 
  left_join(romeo_juliet)

negpos_lines <- bind_rows(neg_lines, pos_lines)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Broadsheet"

col1 <- "#2196F3"
col2 <- "#D32F2F"
col3 <- "#E0D8C9"
col4 <- "#E1EAE3"
col5 <- "#9E948A"

ggplot(rj_sentiment_char, aes(x = line_number)) +
  geom_segment(aes(x = 60, xend = 2980, y = 0), arrow = arrow(length = unit(0.03, "npc")), stat = "unique", color = col5) +
  ggrepel::geom_label_repel(data = negpos_lines, aes(x = line_number, y = total, label = str_wrap(dialogue, 25)), size = 4, direction = "y", family = f2, min.segment.length = 0, fill = alpha(col3, 0.4), label.size = 0.2, segment.colour = col5, segment.size = 0.3, seed = 99) +
  geom_point(aes(y = total, color = total < 0), alpha = 0.1, stroke = 0, size = 3.5) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = rm)) +
  geom_line(aes(y = rm), linewidth = 0.3) +
  scale_color_manual(values = c(col1, col2)) +
  scale_fill_manual(values = c(col1, col2)) +
  coord_flip(clip = "off", ylim = c(-6, 6)) +
  scale_x_reverse() +
  facet_wrap(vars(character), nrow = 1) +
  labs(
    title = "Wherefore art thou, sentiment? The emotional verse of star-cross'd lovers",
    subtitle = "Line-by-line sentiment analysis of Romeo and Juliet's dialogue throughout the play. Points represent individual lines, with<br><span style='color:#2196F3'>**positive**</span> sentiment to the right of the center line and <span style='color:#D32F2F'>**negative**</span> to the left. The smooth curve shows the 20-line rolling<br>average of sentiment. Highlighted quotes are the most <span style='color:#2196F3'>**positive**</span> and <span style='color:#D32F2F'>**negative**</span> lines for each character.",
    caption = "Source: shakespeare.mit.edu · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(family = f2, size = 20),
    strip.background = element_rect(fill = col3),
    panel.background = element_rect(fill = col4),
    plot.title = element_markdown(family = f2, size = 25),
    plot.subtitle = element_markdown(family = f1, size = 12, lineheight = 1.1),
    plot.caption = element_text(hjust = 0)
  )
