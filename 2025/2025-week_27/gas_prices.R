library(tidyverse)
library(legendry)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

weekly_gas_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

wgp <- weekly_gas_prices |> 
  filter(fuel == "gasoline" & grade == "regular" & formulation == "conventional") |> 
  mutate(
    year = year(date),
    week = week(date),
    decade = floor(year / 10) * 10,
    decade_year = year - decade
    ) |> 
  group_by(year) |> 
  mutate(
    year_mean = mean(price, na.rm = TRUE),
    price_diff = price - year_mean
    ) |> 
  ungroup() 

pal <- MetBrewer::met.brewer("Hiroshige", direction = -1)

f1 <- "Sofia Sans Extra Condensed"
f2 <- "LINE Seed Sans"

key <- key_range_manual(
  start = 9,
  end   = 35,
  level = 1,
  name  = "Spring-Summer"
)

ggplot(wgp, aes(week, price)) +
  geom_col(aes(fill = price_diff)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5, labels = c("", "1", "2", "3", "4", "$5")) +
  scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  scale_size_area(max_size = 5) +
  coord_cartesian(expand = FALSE, clip = "off") +
  ggh4x::facet_grid2(vars(decade), vars(decade_year), render_empty = FALSE,     labeller = labeller(decade = function(x) paste0(x, "s"))) +
  labs(
    title = "Average weekly gas price in the US, 1993-2025, by year and decade",
    caption = "Source: Energy Information Administration (EIA) · Graphic: Georgios Karamanis",
    fill = "Price difference between weekly price and yearly mean for regular grade gasoline"
  ) +
  guides(
    x = primitive_bracket(key = key),
    fill = guide_colorbar(title.position = "top")
    ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(2.6, "lines"),
    legend.title = element_text(hjust = 0.5, size = 13),
    legend.text = element_text(hjust = 0.5, size = 11),
    plot.background = element_rect(fill = "#f5f7fb", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = c(paste("grey", seq(97, 87, -2)))),
    strip.text.x = element_blank(),
    strip.text.y = element_text(angle = 0, size = 14, face = "bold"),
    panel.spacing.x = unit(0.2, "lines"),
    panel.spacing.y = unit(2, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legendry.bracket = element_line(linewidth = 0.15),
    plot.title = element_text(size = 15, face = "bold", family = f2, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5, family = f2),
    plot.margin = margin(10, 10, 10, 10)
  )
