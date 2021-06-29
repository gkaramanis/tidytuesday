library(ggforce)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

flix_g <- netflix_titles %>% 
  mutate(
    date_added = mdy(date_added),
    year_added = year(date_added),
    duration = parse_number(duration)
    ) %>% 
  filter(str_detect(listed_in, "LGBTQ")) %>% 
  mutate(
    add_lag = year_added - release_year,
    title = fct_reorder(title, -year_added)
    ) %>% 
  group_by(year_added) %>% 
  mutate(year_label = if_else(row_number() == n(), year_added, NULL)) %>% 
  ungroup()

f1 <- "Fira Sans Compressed"
f2bb <- "Source Serif Pro Black"

pal <- rev(brewer.pal(8, "Dark2"))

ggplot(flix_g) +
  # movies ----
  geom_point(aes(x = year_added, y = title, color = as.factor(year_added)), shape = 18, size = 2) +
  geom_segment(aes(x = release_year, xend = year_added,
                   y = title, yend = title, color = as.factor(year_added))) +
  geom_text(aes(x = release_year, y = title, label = title, color = as.factor(year_added)), hjust = 1, nudge_x = -0.6, family = f1, fontface = "bold", size = 2.5) +
  geom_text(aes(x = 2023, y = title, label = year_label, color = as.factor(year_added)), family = f1, fontface = "bold", hjust = 0, size = 2.5) +
  annotate("text", x = 1964.5, y = 85, label = str_wrap("LGBTQ movies on Netflix by year added to the catalog", 20), hjust = 0, vjust = 1, size = 10, family = f2bb, color = "#E50914", lineheight = 0.95) +
  # legend ----
  annotate("segment", x = 1971, xend = 1985, y = 67, yend = 67, color = "#E50914", size = 1) +
  annotate("point", shape = 18, x = 1985, y = 67, color = "#E50914", size = 4) +
  annotate("text", x = 1969.5, y = 67, label = "Title", color = "#E50914", hjust = 1, family = f1, fontface = "bold", size = 4.5) +
  annotate("text", x = c(1971, 1985), y = 62, label = c("year released", "year added"), color = "#E50914", family = f1, size = 4) +
  annotate("segment", x = c(1971, 1985), xend = c(1971, 1985),
           y = 63.5, yend = 67, color = "#E50914", size = 0.5, linetype = "dashed") +
  annotate("text", x = 1964.5, y = 72, label = "Source: Shivam Bansal/Kaggle · Graphic: Georgios Karamanis", hjust = 0, size = 4, family = f1, color = "grey50") +
  # scales, coord ----
  scale_x_continuous(breaks = seq(1960, 2020, by = 5), minor_breaks = seq(1960, 2020, by = 1), sec.axis = dup_axis()) +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  ### labs ----
  # labs(caption = "Source: Shivam Bansal/Kaggle · Graphic: Georgios Karamanis") +
  # theme ----
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 10, 20, 30),
    plot.background = element_rect(fill = "grey95", color = NA),
    axis.text.x = element_text(size = 8, color = "grey50", margin = margin(7, 0, 7, 0)),
    panel.grid.major.x = element_line(color = "grey83", size = 0.25),
    panel.grid.minor.x = element_line(color = "grey85", size = 0.1),
    plot.caption = element_text(hjust = 0, color = "grey30")
  ) 

ggsave(here::here("temp", paste0("netflix-titles-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 11, width = 8)
