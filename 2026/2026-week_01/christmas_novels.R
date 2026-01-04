library(tidyverse)
library(ggpage)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# christmas_novel_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_authors.csv')
christmas_novel_text <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_text.csv')
# christmas_novels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novels.csv')

acc_raw <- christmas_novel_text |> 
  filter(gutenberg_id == 19337)

acc <- acc_raw |> 
  filter(!is.na(text)) |> 
  filter(row_number() > row_number()[(text == "STAVE ONE")][1]) |> 
  filter(str_detect(text, "STAVE", negate = TRUE)) |> 
  mutate(
    chapter_title = stringr::str_detect(text, "^[^[:lower:]]+$") & text != "EBENEZER SCROOGE." & text != "*     *     *     *     *",
    chapter = cumsum(chapter_title)
    )


joy_words <- c("merry", "happy", "glad", "light", "bright", "joy", 
               "cheerful", "good", "love", "friend", "festive")
cold_words <- c("cold", "frost", "chill", "ice", "dark", "fog", 
                "alone", "solitary", "bitter", "harsh")
wealth_words <- c("money", "poor", "poverty", "generous", "wealthy", 
                  "rich", "penny", "help")

# Colors
pal <- c(
  "cold" = "#673AB7",
  "joy" = "#FF6F00",
  "wealth" = "#009688"
)

acc_pages <- acc |> 
  rename(book = 1) |> 
  filter(!is.na(text)) |> 
  ggpage_build(ncol = 14, lpp = 24, y_space_pages = 25, bycol = FALSE) |> 
  mutate(color = case_when(
    word %in% joy_words ~ pal["joy"],
    word %in% cold_words ~ pal["cold"],
    word %in% wealth_words ~ pal["wealth"],
    chapter_title ~ "grey30",
    TRUE ~ "grey85"
  ))

f1 <- "Familjen Grotesk"

# % of word categories by chapter
p1 <- acc_pages |> 
  filter(!is.na(word)) |> 
  group_by(chapter) |> 
  summarise(
    joy = sum(word %in% joy_words),
    cold = sum(word %in% cold_words),
    wealth = sum(word %in% wealth_words),
    total = n()
  ) |> 
  pivot_longer(cols = c(joy, cold, wealth), names_to = "category", values_to = "count") |> 
  mutate(pct = (count / total) * 100) |> 
  ggplot(aes(x = chapter, y = pct, fill = category)) +
  geom_col(position = position_dodge(), width = 0.8) +
  ggtext::geom_richtext(data = NULL, aes(x = -2, y = 0.1, label = paste0("**Wealth**", "<br>", paste(wealth_words, collapse = "<br>"))), color = pal["wealth"], inherit.aes = FALSE, family = f1, vjust = 1, label.size = 0, fill = NA, size = 3.5) +
  ggtext::geom_richtext(data = NULL, aes(x = -2, y = 0.6, label = paste0("**Joy**", "<br>", paste(joy_words, collapse = "<br>"))), color = pal["joy"], inherit.aes = FALSE, family = f1, vjust = 1, label.size = 0, fill = NA, size = 3.5) +
  ggtext::geom_richtext(data = NULL, aes(x = -2, y = 1.1, label = paste0("**Cold**", "<br>", paste(cold_words, collapse = "<br>"))), color = pal["cold"], inherit.aes = FALSE, family = f1, vjust = 1, label.size = 0, fill = NA, size = 3.5) +
  scale_fill_manual(values = pal) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse(breaks = 1:5, limits = c(-2, 5.4)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

page_n <- acc_pages |> 
  distinct(page, xmin, xmax, ymin, ymax) |> 
  group_by(page) |> 
  reframe(x = mean(c(min(xmin), max(xmax))),
            y = min(ymin))
 
p2 <- ggpage_plot(acc_pages, aes(fill = color), paper.show = TRUE, paper.color = "grey95") +
  geom_text(data = page_n, aes(x, y - 13, label = page), inherit.aes = FALSE, family = f1, size = 2.5, color = "grey50") +
  coord_fixed(expand = FALSE, clip = "off") +
  scale_fill_identity() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  

p1 + p2 +
  plot_layout(widths = c(2, 10)) +
  plot_annotation(
      title = "The spirit of 'A Christmas Carol' in words",
      subtitle = str_wrap("This visualization shows the distribution of words related to wealth, joy, and cold across the chapters of Charles Dickens’s 'A Christmas Carol.' The left chart displays, for each chapter (y-axis), the percentage of words (x-axis) belonging to each category, calculated as the number of matching words divided by the total words in the chapter, using predefined lists for wealth, joy, and cold. The right chart presents a page-by-page layout of the novel, with each word color-coded by theme and the start of each chapter in dark grey.", 140),
    caption = "Source: Project Gutenberg · Graphic: Georgios Karamanis",
    theme = theme_minimal(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.title = element_text(face = "bold", size = 20),
    )
  )
  
