library(tidyverse)
library(camcorder)
library(ggtext)
library(colorspace)
library(patchwork)

gg_record(dir = "temp", device = "png", width = 11, height = 12, units = "in", dpi = 320)

# Read in TidyTuesday data
nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

# Read in Kaggle data
kaggle_emmys <- read_csv(here::here("2021", "2021-week38", "data", "the_emmy_awards.csv"))

# Calculate wins for first dataset
nom_wins <- nominees %>% 
  filter(type == "Winner") %>% 
  filter(year > 2019) %>% 
  mutate(category = str_remove(category, " - \\d{4}")) %>% 
  distinct(year, category, title) %>% 
  count(year, title)

# Calculate wins for second dataset, merge with first dataset, add color category, and calculate categories per year
wins <- kaggle_emmys %>% 
  filter(win) %>% 
  distinct(year, category, title = nominee) %>% 
  count(year, title) %>% 
  rbind(nom_wins) %>% 
  mutate(
    color = case_when(
      n < 5 ~ "0-4 awards",
      n < 10 ~ "5-9 awards",
      TRUE ~ title
    )
  ) %>% 
  group_by(year) %>% 
  mutate(total_n = sum(n)) %>% 
  ungroup()


# Palette
pal <- c("#adb1c4", "#8c92ac", colorspace::lighten(c('#e6194B', '#3cb44b', '#9A6324', '#f032e6', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff'), 0.3))

# Colors
bg_col <- "#FFEFE3"
out_col <- darken(bg_col, 0.6)

# Fonts
f1 = "October Compressed Devanagari"
f2 = "Sharpie"

# Big plot
w <- ggplot(wins, aes(x = -year, y = n, fill = color)) +
  geom_bar(position = "stack", stat = "identity", color = bg_col, size = 0.1) +
  # Label shows with ≥ 10 category wins
  geom_text(aes(label = if_else(n > 9, title, "")), position = position_stack(vjust = 0.5), size = 2.5, family = f1, color = "black") +
  # Background for annotation
  annotate("tile", x = -1959.4, y = c(106.8, 117.0, 128.3), width = 2, height = c(11.6, 8, 13.5), fill = c(pal[1], pal[2], "hotpink")) +
  # Title and annotation text
  geom_richtext(data = NULL, aes(x = -1950, y = 135, label = "<span style='font-size:52pt;font-family:Sharpie'>**Emmy Awards<br><span style='font-size:25pt'>More and more shows win big**</span></span><br>Number of categories won by each show, 1949-2021<br>4 or less, 5 to 9, 10 or more"), hjust = 1, vjust = 1, stat = "unique", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), size = 7, family = f1, color = "grey10") +
  # Caption
  annotate("text", x = -1959.4, y = 99.5, hjust = 1, label = "Data: emmys.com\nGraphic: Georgios Karamanis", family = f1, lineheight = 0.9, color = out_col) +
  # Scales, themes, etc
  scale_x_continuous(breaks = seq(-1950, -2020, -5), labels = seq(1950, 2020, 5)) +
  scale_fill_manual(values = pal) +
  coord_flip(expand = FALSE, clip = "off") +
  theme_void(base_family = f1, base_size = 18) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.y = element_text(margin = margin(10, 10, 10, 10), color = out_col)
  )


# Common code for insets
gg_inset <- function(data) 
  {ggplot(data) +
  geom_line(aes(x = year, y = n), color = out_col) +
  theme_void(base_family = f1) +
  theme(
    axis.text.x = element_text(color = out_col),
    axis.title.x = element_text(margin = margin(5, 0, 0, 0))
  )}

# First inset
big_w <- wins %>% 
  filter(color != "0-4 awards") %>% 
  group_by(year) %>% 
  count() %>% 
  gg_inset() +
  scale_x_continuous(breaks = c(1970, 1990, 2020)) +
  xlab("Shows that won in 5 or more categories") 

# Second inset
tot_c <- wins %>% 
  distinct(year, n = total_n) %>% # Rename total_n column to work with common code
  gg_inset() +
  scale_x_continuous(breaks = c(1960, 1980, 1990, 2020)) +
  xlab("Total categories by year")

# Plot everything
w +
  inset_element(big_w, 0.63, 0.62, 1, 0.82) +
  inset_element(tot_c, 0.63, 0.42, 1, 0.62) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = bg_col)))

