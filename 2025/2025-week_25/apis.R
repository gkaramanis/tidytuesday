library(tidyverse)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# api_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
api_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
# api_logos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
# api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
apisguru_apis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')

common_updated_days <- apisguru_apis |> 
  left_join(api_info |> select(name, provider_name)) |> 
  count(provider_name, updated, sort = TRUE) |> 
  head(5)

common_added_days <- apisguru_apis |> 
  left_join(api_info |> select(name, provider_name)) |> 
  count(provider_name, added, sort = TRUE) |> 
  head(5)

# common_combo_days <- apisguru_apis |> 
#   left_join(api_info |> select(name, provider_name)) |> 
#   count(provider_name, added, updated, sort = TRUE) |> 
#   head(5)

f1 <- "Radio Canada"
f2 <- "Sofia Sans Extra Condensed"

pal <- c(
  "#f8f6f0",
  "#1a1a1a",
  "#2c7be0",
  "#d2691e",
  "#1e4d4d"
)

ggplot() +
  geom_curve(data = apisguru_apis, aes(x = min(decimal_date(updated) - 1), y = decimal_date(added),xend = decimal_date(updated), yend = min(decimal_date(added)) - 1, color = case_when(
    added %in% common_added_days$added ~ pal[3],
    updated %in% common_updated_days$updated ~ pal[4],
    TRUE ~ pal[2]
  )), curvature = -0.35, linewidth = 0.1, alpha = 0.6, ncp = 10) +
  # Points for common update days
  geom_point(data = common_update_days, aes(x = decimal_date(updated), y = min(decimal_date(apisguru_apis$added)) - 1.1), color = pal[4], shape = "▲", size = 2.5) +
  ggrepel::geom_text_repel(data = common_update_days, aes(x = decimal_date(updated), y = min(decimal_date(apisguru_apis$added)) - 1.1, label = paste0(date(updated), "\n", provider_name, " (", n, ")")), color = pal[4], nudge_y = -0.7, lineheight = 0.9, family = f2, seed = 99) +
  # Points for common added days
  geom_point(data = common_added_days, aes(y = decimal_date(added), x = min(decimal_date(apisguru_apis$updated)) - 1.05), color = pal[3], shape = "▶", size = 2.5) +
  ggrepel::geom_text_repel(data = common_added_days, aes(y = decimal_date(added), x = min(decimal_date(apisguru_apis$updated)) - 1.05, label = paste0(date(added), "\n", provider_name, " (", n, ")")), color = pal[3], hjust = 1, direction = "y", nudge_x = -1.2, lineheight = 0.9, family = f2, seed = 99) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(breaks = 2015:2023) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  labs(
    title = "API lifecycle patterns in APIs.guru",
    subtitle = "Curves connect when APIs were added to the directory (y-axis) with their last update date (x-axis). Highlighted points show days when many APIs from the same provider were {.#1e6bd2 **added**} or {.#d2691e **updated**} simultaneously.",
    caption = "Data: APIs.guru · Graphic: Georgios Karamanis",
    x = "Date of last **update**",
    y = "Date of **addition** to APIs.guru"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[1], color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = pal[4], linewidth = 0.2, linetype = "dotted"),
    axis.title.x = element_marquee(margin = margin(t = 10), color = pal[5]),
    axis.title.y = element_marquee(margin = margin(r = 10), color = pal[5]),
    axis.text = element_text(color = pal[5]),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_marquee(margin = margin(b = 10), width = 0.99),
    plot.margin = margin(20, 20, 20, 20)
  )
  