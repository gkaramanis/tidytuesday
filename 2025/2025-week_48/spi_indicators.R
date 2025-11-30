library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 13, height = 8, units = "in", dpi = 320)

spi_indicators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')

spii <- spi_indicators |> 
  filter(!is.na(overall_score)) 

f1 <- "Source Serif Pro"
f2 <- "Input Mono Compressed"
f3 <- "Sofia Sans Extra Condensed"

highlight_col <- "#2C7FB8"  
muted_col <- "#E9ECEF"      
highlight_label_col <- "#0B4C6B" 
muted_label_col <- "#7A7A7A"

# Common theme
theme_set(
  theme_bw(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text = element_text(size = 10, family = f2),
      axis.title = element_blank(),
      strip.text = element_text(size = 10.5, family = f3, hjust = 0, vjust = 0, face = "bold")
    )
)

# Compare Europe & Central Asia to the other regions
p1 <- ggplot(spii) +
  geom_smooth(aes(year, overall_score, group = region, fill = if_else(region == "Europe & Central Asia", "Europe & Central Asia", "Other regions")), size = 1.2, alpha = 0.5, color = NA) +
  geomtextpath::geom_textsmooth(aes(year, overall_score, group = region, label = region, color = if_else(region == "Europe & Central Asia", "Europe & Central Asia", "Other regions"), fontface = if_else(region == "Europe & Central Asia", "bold", "plain")), vjust = 0.3, family = f1, hjust = seq(0.1, 0.9, length.out = 560)) +
  scale_fill_manual(values = c("Europe & Central Asia" = highlight_col, "Other regions" = muted_col)) +
  scale_color_manual(values = c("Europe & Central Asia" = highlight_label_col, "Other regions" = muted_label_col)) +
  scale_x_continuous(breaks = range(spii$year)) +
  scale_y_continuous(limits = c(10, 100)) +
  labs(
    title = "SPI data: Most Europe & Central Asia countries score high, but a few lag behind.",
    subtitle = str_wrap("Europe & Central Asia ranks among the top regions in SPI scores, just below North America. Within the region, high-income (↑) countries such as Denmark, Finland, and Poland consistently achieve the highest scores, while lower (↘) and upper middle (↗) income  countries like Uzbekistan, Moldova, and Georgia show lower and more varied performance.", 180),
    caption = "Source: World Bank · Graphic: Georgios Karamanis"
  ) +
  theme(
    legend.position = "none",
    panel.background = element_rect(color = "black", linewidth = 1),
    plot.title = element_text(size = 20, face = "bold"),
    plot.caption = element_text(hjust = 0)
  )
  
# Compare each Europe & Central Asia country to all the countries in the region
p2 <- spii |> 
  filter(region == "Europe & Central Asia") |> 
  mutate(country_label = case_when(
    income == "High income" ~ paste0(country, "↑"),
    income == "Upper middle income" ~ paste0(country, "↗"),
    income == "Lower middle income" ~ paste0(country, "↘")
  )) |> 
  ggplot() +
  geom_path(aes(year, overall_score, group = country), size = 0.7, color = highlight_label_col) +
  gghighlight::gghighlight(unhighlighted_params = list(linewidth = 0.15, alpha = 0.2, color = highlight_col)) +
  scale_x_continuous(breaks = range(spii$year)) +
  scale_y_continuous(limits = c(10, 100), breaks = c(50, 100)) +
  facet_wrap(vars(str_wrap(country_label, 10))) +
  theme(
    axis.text.x = element_text(hjust = c(0, 1), size = 8),
    axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = NA, color = NA)
  )

p1 + p2
