library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# financing_schemes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/financing_schemes.csv')
health_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/health_spending.csv')
spending_purpose <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/spending_purpose.csv')

cc <- ISOcodes::UN_M.49_Regions |> 
  filter(!is.na(Parent)) |> 
  separate_longer_delim(Children, delim = ", ") |>
  left_join(ISOcodes::UN_M.49_Countries, by = c("Children" = "Code")) |> 
  select(region_code = Code, region = Name.x, country = Name.y, iso3_code = ISO_Alpha_3) |>
  filter(!is.na(country))

prev_care_europe <- spending_purpose |>
  filter(spending_purpose == "Preventive care") |>
  filter(indicator_code == "hc6_che") |>
  left_join(cc) |>
  filter(str_detect(region, "Europe")) |> 
  mutate(
    country_name = case_when(
      str_detect(country, "United Kingdom") ~ "United Kingdom",
      str_detect(country, "Russian F") ~ "Russia",
      str_detect(country, "Moldova") ~ "Moldova",
      str_detect(country, "Netherlands") ~ "Netherlands",
      str_detect(country, "Bosnia") ~ "Bosnia & Herz.",
      str_detect(country, "Macedonia") ~ "N. Macedonia",
      TRUE ~ country_name
    ),
    region = fct_relevel(region, "Northern Europe", "Eastern Europe", "Western Europe", "Southern Europe")
    ) |>
  arrange(country_name, year) |>
  group_by(country_name) |>
  mutate(pp_change = value - lag(value)) |>
  ungroup()

f1 <- "Karst"
f2 <- "Metropolis"

region_colors <- c(
  "Northern Europe" = "#4a7fa5",
  "Eastern Europe"  = "#c4622d",
  "Western Europe"  = "#5a8a5a",
  "Southern Europe" = "#b5944a"
)

prev_care_europe |> 
  filter(!is.na(pp_change)) |> 
  group_by(region) |>
  group_map(~ {
    reg_col <- region_colors[.y$region]
    
    ggplot(.x, aes(year, pp_change, color = country_name)) +
      geom_line(linewidth = 0.7) +
      gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_params = list(linewidth = 0.2, alpha = 0.25, colour = colorspace::desaturate(reg_col, 0.5))) +
      geom_point(size = 0.3) +
      scale_color_manual(values = setNames(rep(reg_col, n_distinct(.x$country_name)), unique(.x$country_name))) +
      scale_x_continuous(breaks = c(2017, 2021, 2023), labels = c(2017, "", 2023)) +
      scale_y_continuous(limits = c(-5, 7.5), breaks = seq(-5, 7.5, 5)) +
      facet_wrap(vars(country_name), nrow = 3) +
      labs(
        title = .y$region
        ) +
      theme_minimal(base_family = f2) +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#f8f8f6", color = NA),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(size = 8, hjust = 0),
        axis.text = element_text(size = 6),
        axis.title = element_blank(),
        plot.title = element_text(size = 12, family = f1, face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
      )
  }) |>
  wrap_plots(ncol = 2, nrow = 2) +
  plot_annotation(
    title = "Europe's regions' response to COVID through preventive care spending",
    subtitle = str_wrap("Percentage point change in preventive care's share of current health expenditure, year on year, 2017–2023. 2021 marks the peak COVID vaccination year. Western Europe saw the largest increases on average, but with the widest spread between countries. Northern Europe spiked clearly in 2021 and recovered together, more consistently than other regions. Eastern Europe was split between countries with large spikes and those that barely moved. Southern Europe had the smallest overall response, with similar magnitudes across countries but varied timing.", 140),
    caption = "Source: WHO Global Health Expenditure Database (GHED) · Graphic: Georgios Karamanis",
    theme = theme(
      plot.title = element_text(size = 18, family = f1, face = "bold"),
      plot.subtitle = element_text(size = 10, family = f2),
      plot.caption = element_text(size = 8, family = f2, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
     )
    )

record_polaroid()
