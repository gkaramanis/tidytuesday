library(tidyverse)
library(eulerr)
library(grid)
library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 8, height = 8, dpi = 320)

dead_sea_scrolls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-15/dead_sea_scrolls.csv')

dsc_mat <- dead_sea_scrolls |>
  filter(
    !is.na(biblical_book),
    canon_status %in% c("Protocanonical", "Deuterocanonical"),
    language %in% c("Hebrew", "Aramaic", "Greek")
  ) |>
  distinct(biblical_book, language) |>
  # 5Q4 (Amos) has no Leon Levy archive record, so its language is empty.
  # Wikipedia's Cave 5 table lists 5QAmos as Hebrew.
  add_row(biblical_book = "Amos", language = "Hebrew") |>
  mutate(present = TRUE) |>
  pivot_wider(names_from = language, values_from = present, values_fill = FALSE) |>
  column_to_rownames("biblical_book")

set.seed(989)

f1 <- "Epilogue Static"

ep <- systemfonts::system_fonts()
ep <- ep[!ep$variable, ]

systemfonts::register_font(
  name  = f1,
  plain = ep$path[ep$name == "Epilogue-Regular"][1],
  bold  = ep$path[ep$name == "Epilogue-Bold"][1]
)

p <- plot(
  euler(dsc_mat, shape = "rotated_rectangle"),
  # legend = list(side = "top", ncol = 3, nrow = 1, fontfamily = f1, fontsize = 14, fontface = "bold"),
  quantities = list(fontfamily = f1, fontsize = 20),
  labels = list(fontfamily = f1, fontsize = 15),
  fills = c(
    "Hebrew"                = "#E3D9C6",
    "Greek"                 = "#D08B6E",
    "Aramaic"               = "#7BA3A8",
    "Hebrew&Aramaic"        = "#AFBEB7",
    "Hebrew&Greek"          = "#DAB29A",
    "Hebrew&Aramaic&Greek"  = "#BAAD9F"
  )
)

ggplotify::as.ggplot(p) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f1, ink = "#4A4741") +
  labs(
    title = "Dead Sea Scrolls: Biblical books by language",
    subtitle = "All 28 canonical books identified among the scrolls, placed by the primary language of the manuscripts that preserve them. **Where the shapes overlap**, the same book survives in copies of **more than one language.** No book survives in Aramaic alone.",
    caption = "Source: Leon Levy Dead Sea Scrolls Digital Library  & Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = marquee::element_marquee(width = 0.95, lineheight = 1.1, margin = margin(t = 7, b = 20)),
    plot.caption = element_text(margin = margin(t = 20)),
    plot.margin = margin(15, 15, 15, 15)
  )
