library(tidyverse)
library(scales)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 7, units = "in", dpi = 320)

# pi_digits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-24/pi_digits.csv')

pi_uses <- tribble(
  ~label, ~digits,
  "Everyday math", 5,
  # "NASA interplanetary navigation", 15,
  "Voyager 1 trajectory (error < finger width)", 16,
  "Observable universe circumference (±hydrogen atom)", 38,
  "Max any practical science ever needs", 100,
  "This TidyTuesday dataset", 1e6,
  "Google world record (Emma Haruka Iwao, 2019)", 31.4e12,
  "Guinness World Record (KIOXIA/Linus, 2025)", 300e12,
  "Unofficial record (StorageReview, 2025)", 314e12
) |> 
  mutate(label = fct_inorder(label))


f1 <- "Karst"
f2 <- "Metropolis"

ggplot(pi_uses, aes(x = label, y = digits, label = paste0(label, ": ", number(digits, scale_cut = cut_short_scale())), fill = str_detect(label, "TidyTuesday"))) +
  geom_col() +
  geom_text(data = . %>% filter(digits < 5e6), hjust = 0, nudge_y = 0.2, fontface = "bold") +
  geom_text(data = . %>% filter(digits > 5e6), hjust = 1, nudge_y = -0.2, color = "white", fontface = "bold") +
  coord_flip(expand = FALSE) +
  scale_y_log10(labels = label_log(), breaks = c(1e3, 1e6, 1e9, 1e12)) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = c("#62CBDE", "#EB9135"), guide = "none") +
  labs(
    title = toupper("How many digits of pi do we need?"),
    caption = "Graphic: Georgios Karamanis",
    y = "Digits (log scale)"
  ) +
  theme_void(base_family = f1, base_size = 15) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.x = element_text(family = f2, face = "bold"),
    axis.text.x = element_text(family = f2, size = 11, margin = margin(t = 5, b = 10)),
    plot.margin = margin(15, 20, 10, 20),
    plot.title = element_text(face = "bold", size = 28, hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(family = f2, size = 11)
  )


record_polaroid()
