library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

sechselaeuten <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv')

f1 <- "Porpora"
f2 <- "Onest"

pal <- MetBrewer::met.brewer("Johnson", direction = -1)

ggplot(sechselaeuten) +
  geomtextpath::geom_texthline(yintercept = 19, linetype = "dotted", label = "19°C", color = "grey50", hjust = 0.15, family = f1, size = 3.5) +
  ggforce::geom_link(aes(x = year, xend = year, y = tre200mn, yend = tre200mx, color = after_stat(y)), linewidth = 2, lineend = "round") +
  scale_color_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 19)) +
  ggnewscale::new_scale_color() +
  geom_point(aes(x = year, y= tre200m0, color = record)) +
  scale_color_manual(values = c("black", "red2")) +
  # annotations
  geom_text(data = sechselaeuten |> filter(year == min(year)), aes(x = year, y = tre200mx, label = "Highest temperature"), hjust = 0, nudge_x = 1.5, size = 3) +
  geom_text(data = sechselaeuten |> filter(year == min(year)), aes(x = year, y = tre200mn, label = "Lowest temperature"), hjust = 0, nudge_x = 1.5, size = 3) +
  geom_text(data = sechselaeuten |> filter(year == min(year)), aes(x = year, y = tre200m0, label = "Mean summer\ntemperature"), hjust = 0, nudge_x = 1.5, size = 3, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(1925, 2025, 10), minor_breaks = 1923:2025, guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(breaks = seq(5, 35, 5), minor_breaks = 5:35, position = "right", labels = function(x) paste0(x, " °C")) +
  labs(
    title = "The Böögg can’t predict the weather, let alone the warming",
    subtitle = str_wrap("Everyone knows the Böögg cannot really predict Zurich’s summer weather, so no one would expect it to foresee how much hotter the city’s summers have become. In the past 11 years, average temperatures rose above 19 °C eight times, and highs surpassed 30 °C every summer.", 140),
    caption = "Source: github.com/philshem & data.geo.admin.ch · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(lineend = "round", color = "grey50"),
    axis.minor.ticks.length.x = unit(0.05, "lines"),
    axis.text.y = element_text(face = "bold", family = f2),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", family = f2)
  )

record_polaroid()
