library(tidyverse)
library(ggfx)
library(marquee)
library(ggforce)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

bl_funding <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')

blf <- bl_funding |> 
  select(year, nominal_gbp_millions, total_y2000_gbp_millions) |> 
  mutate(
    ymin = total_y2000_gbp_millions,
    ymax = nominal_gbp_millions,
    label = case_when(
      year == 2006 ~ "2006: funding peak. Nominal and real values hit 159.2 and 144.8 million pounds, respectively, before declining.",
      year == 2013 ~ "2013: digital deposit. The British Library begins collecting digital material.",
      year == 2021 ~ "2021: temporary bump. Funding rises briefly from decarbonisation and lottery grants.",
      TRUE ~ NA
    )
  )

f1 <- "Golos UI"

ggplot() +
  as_reference(
    ggh4x::stat_difference(data = blf, aes(x = year, ymin = ymin, ymax = ymax), alpha = 0.5),
    id = "diff"
  ) +
  with_blend(
    geom_abline(aes(slope = 1, intercept = seq(-1950, -1850, 1)), color = "white", linewidth = 0.5, alpha = 0.6),
    bg_layer = "diff",
    blend_type = "atop"
  ) +
  geom_line(data = blf, aes(x = year, y = ymax, color = "#2980B9"), linewidth = 1.5) +
  geom_line(data = blf, aes(x = year, y = ymin, color = "#E67E22"), linewidth = 1.3, linetype = "dashed") +
  geom_mark_rect(data = blf |> filter(!is.na(label)), aes(x = year, y = ymin, label = label, group = factor(year)), fill = "#f3f4f6", expand = 0, color = "#22223B", label.fontsize = 12, label.family = f1, label.colour = "#22223B", label.fill = alpha("#f3f4f6", 0.7), label.width = unit(10, "lines")) +
  scale_fill_manual(values = c("#E67E22", "#2980B9")) +
  scale_color_identity() +
  scale_x_continuous(minor_breaks = min(bl_funding$year):max(bl_funding$year), expand = expansion(add = 0.05)) +
  scale_y_continuous() +
  labs(
    title = "Real value of British Library funding continues to shrink",
    subtitle = "Annual funding in millions of pounds from 1998 to 2023, showing nominal amounts ({.#2980B9 **solid line**}) with modest recent increases. The real value, adjusted to 2000 pounds ({.#E67E22 **dashed line**}), steadily erodes due to inflation, creating a widening gap over time.",
    caption = "Source: British Library and Bank of England (via Andy Jackson and David Rosenthal) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_size = 15, base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(face = "bold", size = 20),
    plot.title.position = "plot",
    plot.subtitle = element_marquee(width = 0.95, margin = margin(-5, 0, 25, 0)),
    plot.caption = element_text(size = 11, margin = margin(15, 0, 0, 0))
  )
