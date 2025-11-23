library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 10, units = "in", dpi = 320)

holmes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-18/holmes.csv')

excl_holmes <- holmes |> 
  filter(!is.na(text)) |> 
  reframe(
    .by = book,
    words_n = sum(str_count(text, boundary("word"))),
    excl_n = sum(str_count(text, ("\\!"))),
    excl_per_1000_words = excl_n / words_n * 1000
    ) |> 
  arrange(excl_per_1000_words) |> 
  mutate(
    ymin = cumsum(lag(words_n, default = 0)),
    ymax = ymin + words_n,
    # add space
    ymin = ymin + row_number() * 7000,
    ymax = ymax + row_number() * 7000
  )

f1 <- "Sofia Sans Extra Condensed"
f2 <- "Charter"

ggplot(excl_holmes) +
  geom_rect(aes(xmin = 0, xmax = excl_per_1000_words, ymin = ymin, ymax = ymax), fill = "black", color = NA) +
  geom_rect(aes(xmin = -0.1, xmax = -0.3, ymin = ymin, ymax = ymax), fill = "black", color = NA) +
  geom_text(aes(x = -0.4, y = (ymin + ymax) / 2, label = book), family = f1, hjust = 1, size = 3.5) +
  scale_y_continuous(expand = 0) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Sherlock!",
    subtitle = str_wrap("Number of exclamation marks per 1,000 words in each Sherlock Holmes book. Bar width represents the total word count for each book", 75),
    caption = "Source: sherlock R package · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 13) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = f2, face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.margin = margin(10, 10, 10, 120)
  )

record_polaroid()
