library(tidyverse)
library(ggHoriPlot)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

cases_month <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')

f1 <- "Host Grotesk"
f2 <- "Iosevka"

plot_region <- function(x) {
  df <- measles_regions |> 
    filter(region == x) |> 
    filter(!is.na(measles_total)) |> 
    filter(year >= 2020) |> 
    mutate(
      c = case_when(
        str_detect(country, "Democratic Republic of the Congo") ~ "DR Congo",
        str_detect(country, "Russian F") ~ "Russia",
        TRUE ~ country
      ),
      c = fct_lump_n(c, 3, w = measles_total),
    )
  
  pal <- MetBrewer::met.brewer("Tam", direction = -1)[c(1, 3, 5, 7)]
  
  p <- ggplot(df, aes(x = month, y = measles_total, fill = c)) +
    geom_col() +
    scale_x_continuous(breaks = 1:12, labels = str_sub(month.abb, 1, 1)) +
    scale_y_continuous(labels = scales::label_number(), limits = c(0, 20000)) +
    scale_fill_manual(values = pal) +
    facet_grid(vars(region), vars(year)) +
    labs(fill = x) +
    theme_minimal(base_family = f1) +
    theme(
      legend.position = "right",
      legend.justification = 0,
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.85, "lines"),
      legend.title = element_text(size = 13),
      plot.background = element_rect(fill = "grey98", color = NA),
      axis.title = element_blank(),
      strip.text.x = if(x == "AFR"){element_text(face = "bold", size = 10)} else {element_blank()},
      axis.text = element_text(family = f2),
      axis.text.x = if(x == "WPR"){element_text(color = "grey20")} else {element_text(color = "grey70")},
      axis.text.y = if(x == "AFR"){element_text(color = "grey20")} else {element_text(color = "grey70")},
      strip.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

plot_list <- lapply(unique(cases_month$region), plot_region)

wrap_plots(plot_list, ncol = 1) &
  plot_annotation(
    title = "Top 3 countries with the most measles cases in each WHO region",
    caption = "Source: Provisional data based on monthly data reported to WHO (Geneva) as of June 2025 · Graphic: Georgios Karamanis",
    theme = theme(
      plot.title = element_text(family = f1, size = 16, face = "bold"),
      plot.caption = element_text(family = f1, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    )
  )

