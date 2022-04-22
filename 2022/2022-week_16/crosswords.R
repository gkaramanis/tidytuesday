library(tidyverse)
library(camcorder)
library(lubridate)
library(ggtext)

gg_record(dir = "tidytuesday-temp", device = "png", width = 16, height = 14, units = "in", dpi = 320)

times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv') 

times_sq <- times %>% 
  filter(str_detect(tolower(puzzle_name), "jumbo")) %>%
  filter(str_sub(clue_number, 1, 1) %in% 1:9) %>% 
  mutate(
    word = str_remove_all(answer, " |-|'"),
    word = word,
    word_l = nchar(word),
    month = month(puzzle_date, label = TRUE),
    month = fct_rev(month),
    year = year(puzzle_date)
    ) %>% 
  filter(word_l > 2) %>% 
  mutate(
    clue_d = str_sub(clue_number, -1),
    clue_n = parse_number(clue_number)
  )  %>% 
  filter(clue_n < 100) %>% 
  filter(!is.na(year)) %>% 
  group_by(year, month) %>% 
  filter(word_l == max(word_l)) %>% 
  ungroup() %>% 
  select(year, month, word, word_l) %>%
  arrange(year, month)
 
annot <- tribble(
  ~year, ~month, ~label,
  2016, "Jan", "<br><span style = 'font-size:54px;'>Longest</span><br>words in the Times Jumbo Cryptic Crosswords",
  2016, "Mar", "Source: Cryptic Crossword Clues\nGraphic: Georgios Karamanis"
  ) 

annot2 <- tribble(
  ~year, ~month, ~label,
  2021, "Oct", "Length of answers as number of characters by month and year"
  ) 

# bbox <- tribble(
#   ~year, ~month_min, ~month_max, ~l_min, ~l_max,
#   2021, "Oct", "Dec", 0, 23
# )

# Plot
f1 <- "Input Mono"
f2 <- "American Typewriter"

ggplot(times_sq) +
  # Bars with words
  geom_col(aes(x = month, y = word_l, group = word, fill = word_l), position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(x = month, y = word_l, label = word), color = "white", position = position_dodge2(width = 0.9, preserve = "single"), family = f1, hjust = 1.03) +
  # Title and caption
  geom_textbox(data = annot, aes(x = month, y = 11.5, label = label, group = NA), family = f2, fontface = c("bold", "plain"), size = c(8.5, 3.5), halign = 0.5, vjust = 1, box.margin = unit(c(0, -1, 0, -1), "line"), box.r = unit(0, "line"), fill = "black", color = "white", box.color = "black", nudge_x = c(0.33, -0.2)) +
  geom_textbox(data = annot2, aes(x = month, y = 11.5, label = label, group = NA), family = f2, size = 5, halign = 0.5, vjust = 1, box.margin = unit(c(0, -1, 0, -1), "line"), box.padding = unit(c(4, 0.5, 8, 0.5), "line"), box.r = unit(0, "line"), fill = "black", color = "white", box.color = "black", nudge_x = 0) +
  # geom_rect(data = bbox, aes(xmin = month_min, xmax = -Inf, ymin = l_min, ymax = l_max), fill = "black", color = "black") +
  # Scales
  scale_fill_gradientn(colours = c("grey70", "black")) +
  scale_y_continuous(breaks = seq(min(times_sq$word_l), max(times_sq$word_l), 2), minor_breaks = seq(min(times_sq$word_l), max(times_sq$word_l), 1), expand = expansion(0)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0.03))) +
  # Other stuff
  coord_flip() +
  facet_wrap(vars(year), nrow = 1) +
  theme_bw(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 26, color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "black", size = 0.1),
    panel.grid.minor.x = element_line(color = "black", size = 0.05),
    panel.spacing.x = unit(1, "line"),
    panel.border = element_rect(color = "black")
  )

