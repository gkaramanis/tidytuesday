library(tidyverse)
library(shadowtext)
library(patchwork)
library(camcorder)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')

top_movies_img <- png::readPNG(here::here("2025/2025-week_31/img/NFLX_H12025_EngagementReport_Top10Movies.png"), native = TRUE)

top_shows_img <- png::readPNG(here::here("2025/2025-week_31/img/NFLX_H12025_EngagementReport_Top10Shows.png"), native = TRUE)

dim_h <- dim(top_movies_img)[1] * 2
dim_w <- dim(top_movies_img)[2] * 2

gg_record(dir = "tidytuesday-temp", device = "png", width = dim_w, height = dim_h, units = "px", dpi = 320)

top_movies <- movies |>
  filter(report == "2025Jan-Jun") |>
  slice_max(order_by = views, n = 10) |>
  mutate(title = fct_reorder(title, views))

top_shows <- shows |>
  filter(report == "2025Jan-Jun") |>
  slice_max(order_by = views, n = 10) |>
  mutate(title = fct_reorder(title, views))

f1 <- "Outfit"
col_red <- "#ff000d"

ggplot(top_movies, aes(views, title)) +
  geom_col(color = col_red, fill = NA, linewidth = 1, width = 0.85) +
  geom_shadowtext(aes(x = 2e6, label = title), hjust = 0, vjust = -0.4, color = col_red, family = f1, size = 3.5, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  geom_shadowtext(aes(x = views - 2e6, label = scales::number(janitor::round_half_up(views, -6), scale_cut = scales::cut_short_scale())), hjust = 1, color = col_red, family = f1, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  annotate("text", x = 0, y = 0, label = "Source: Netflix · Graphic: Georgios Karamanis", color = col_red, family = f1, hjust = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.095, 0.155))) +
  scale_y_discrete(expand = expansion(mult = 0.175)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  inset_element(top_movies_img, 0, 0, 1, 1, on_top = FALSE)

ggplot(top_shows, aes(views, title)) +
  geom_col(color = col_red, fill = NA, linewidth = 1, width = 0.85) +
  geom_shadowtext(aes(x = 2e6, label = title), hjust = 0, vjust = -0.4, color = col_red, family = f1, size = 3.5, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  geom_shadowtext(aes(x = views - 2e6, label = scales::number(janitor::round_half_up(views, -6), scale_cut = scales::cut_short_scale())), hjust = 1, color = col_red, family = f1, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  annotate("text", x = 0, y = 0, label = "Source: Netflix · Graphic: Georgios Karamanis", color = col_red, family = f1, hjust = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.095, 0.155))) +
  scale_y_discrete(expand = expansion(mult = 0.175)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  inset_element(top_shows_img, 0, 0, 1, 1, on_top = FALSE)


# Measured bar lengths in Netflix report charts
movies_data <- top_movies |> 
  select(title, views) |> 
  mutate(
    px = c(975, 824, 757, 668, 657, 523, 519, 503, 498, 498),
    expected_px = views * px[1] / views[1]
    )

shows_data <- top_shows |> 
  select(title, views) |> 
  mutate(
    px = c(976, 842, 523, 487, 464, 448, 434, 434, 434, 406),
    expected_px = views * px[1] / views[1]
  )

ms_data <- bind_rows(
  movies_data %>% mutate(type = "Movies"),
  shows_data %>% mutate(type = "Shows")
)

ggplot(ms_data, aes(views, px, color = type, fill = type)) +
  geom_line(aes(y = expected_px), linetype = "dashed") +
  geom_point(size = 3, shape = 21, alpha = 0.8, stroke = 0.7) +
  geom_text(data = . %>% filter(px > 600), aes(label = title), nudge_x = -5e6, hjust = 1, family = f1, size = 3, check_overlap = TRUE) +
  geom_text(data = . %>% filter(px < 600), aes(label = title), nudge_x = 5e6, hjust = 0, family = f1, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_color_manual(values = c("Movies" = "#8482F7", "Shows" = "#62D6BD")) +
  scale_fill_manual(values = c("Movies" = "#8482F7", "Shows" = "#62D6BD")) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(type)) +
  labs(
    subtitle = str_wrap("Measured bar lengths from Netflix's H1 2025 report images vs. actual view counts. The dashed line shows proportionally correct bar lengths based on the top-ranked title in each category", width = 100),
    caption = "Source: Netflix · Graphic: Georgios Karamanis",
    x = "Views",
    y = "Bar length (pixels)"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey19", color = NA),
    panel.grid = element_blank()
  )
