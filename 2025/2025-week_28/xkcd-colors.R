library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.5, height = 10, units = "in", dpi = 320)

answers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv')
color_ranks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')
users <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv')

answers_xy <- answers |> 
  left_join(users) |>
  filter(spam_prob < 0.05) |>
  filter(!is.na(colorblind)) |>
  filter(!is.na(y_chromosome)) |>
  mutate(
    lab = map(hex, ~as(colorspace::hex2RGB(.x), "LAB")@coords[1,]),
    L = map_dbl(lab, ~.x[1]),
    a = map_dbl(lab, ~.x[2]), 
    b = map_dbl(lab, ~.x[3])
  )

top_xy <- color_ranks |> 
  mutate(
    lab = map(hex, ~as(colorspace::hex2RGB(.x), "LAB")@coords[1,]),
    L = map_dbl(lab, ~.x[1]),
    a = map_dbl(lab, ~.x[2]), 
    b = map_dbl(lab, ~.x[3])
  )

f1 <- "Familjen Grotesk"

ggplot() +
  geom_point(data = answers_xy, aes(a, b, color = hex), size = 0.1) +
  geom_point(data = top_xy, aes(a, b, fill = hex), color = "black", size = 1, stroke = 0.1, shape = 21) +
  geom_point(data = top_xy |> filter(rank <= 20), aes(a, b, fill = hex), color = "white", size = 2, stroke = 0.3, shape = 21) +
  ggrepel::geom_text_repel(data = top_xy |> filter(rank <= 20), aes(a, b, label = color), bg.color = "grey30", color = "white", family = f1) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  labs(
    title = "xkcd color survey results in LAB color space",
    subtitle = "Almost 317 000 replies filtered for low spam probability and complete user data, with top 20 most common colors highlighted",
    caption = "Source: xkcd color survey · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey30", color = NA),
    panel.background = element_rect(fill = NA, color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 12, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(color = "white", hjust = 0.5, size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )
