library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 6, units = "in", dpi = 320)

# nde_experiences <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-07-21/nde_experiences.csv')

# Use data from modified cleaning script
nde_experiences <- read_csv(here::here("2026", "2026-week_30", "data", "nde_experiences.csv"))

nde_md <- nde_experiences |> 
  mutate(
    exp_y = year(ymd(as.Date(exp_date))),
    exp_m = month(ymd(as.Date(exp_date)), label = TRUE),
    exp_d = day(ymd(as.Date(exp_date)))
  ) |> 
  filter(!is.na(exp_m)) |> 
  count(exp_m, exp_d)

f1 <- "Iosevka Charon"

ggplot(nde_md, aes(x = exp_d, y = exp_m, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, alpha = log(n)), color = "white") +
  MetBrewer::scale_fill_met_c("Tam") +
  scale_alpha_continuous(range = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1, 7, 14, 21, 28, 31)) +
  scale_y_discrete(limits = rev) +
  coord_fixed(expand = FALSE) +
  labs(
    title = "When people say they almost died",
    subtitle = str_wrap("This heatmap counts near-death experiences scraped from the Near Death Experience Research Foundation by the day and month each person gave as the date of their experience. Of the accounts collected, only the roughly 5,570 with a valid date are shown. Darker squares hold more reports. Most of the calendar stays low and even, but a few dates break sharply from it, a pattern that points to how the dates were recorded rather than when the experiences happened.", 118),
    caption = "Source: Near Death Experience Research Foundation · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 1),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(margin = margin(t = 5, b = 20))
  )
