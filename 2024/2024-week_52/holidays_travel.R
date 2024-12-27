library(tidyverse)
library(colorspace)
library(legendry)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

monthly_passengers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv') %>% 
  janitor::clean_names()

domestic_international <- monthly_passengers %>% 
  mutate(
    domestic_ratio = domestic / international,
    domestic_ratio_log = log10(domestic_ratio),
    month = factor(month),
    year = factor(year)
    ) %>%
  filter(!is.na(domestic_ratio)) %>%
  filter(iso3 %in% c("AUS", "CHN", "DEU", "GBR", "GRC", "NOR", "SWE", "THA", "USA")) %>%
  group_by(iso3) %>% 
  mutate(
    mean_ratio = round(exp(mean(log(domestic_ratio))), 1),
    ratio_direction = if_else(mean_ratio > 1, 
                            "x more domestic", 
                            "x more international"),
    display_ratio = if_else(mean_ratio > 1, 
                           mean_ratio, 
                           round(1/mean_ratio, 1))
    ) %>% 
  ungroup() %>% 
  mutate(
    country = countrycode::countrycode(iso3, origin = "iso3c", destination = "country.name"),
    country_label = paste0(
      "**", country, ":** ", display_ratio, "", ratio_direction
      ),
    country_label = fct_reorder(country_label, mean_ratio)
    )

f1 <- "Fantasque Sans Mono"
f2 <- "Graphik"  

brackets <- key_range_manual(
  start = c(-1.05, 0.01),
  end   = c(-0.01, 1.13), 
  name  = c("more international", "more domestic")
  ) %>% 
  primitive_bracket(bracket = "square")


ggplot(domestic_international) +
  geom_tile(aes(month, year, fill = domestic_ratio_log)) +
  geom_vline(xintercept = c(5.5, 8.5), linewidth = 0.5, color = "grey99", linetype = "dotted") +
  scale_x_discrete(
    labels = c("Jan", "", "Mar", "", "May", "", "Jul", "", "Sep", "", "Nov", ""),
    guide = guide_axis_nested(
      regular_key = "auto",
      key = key_range_manual(c(5.5), c(8.5), c("N. hemisphere summer"))
    )) +
  scale_y_discrete(limits = rev, labels = c("2019", "", "", "", "2015", "", "", "", "2011", "")) +
  scale_fill_continuous_diverging("Purple-Green",
                              breaks = c(-1, -0.5, 0, 0.5, 1),
                              limits = c(-1.1, 1.18),
                              mid = 0,
                              labels = c("10 times", 
                                       "3 times",
                                       "Equal",
                                       "3 times",
                                       "10 times"),
                              guide = guide_colbar(second_guide = brackets)
                              ) +
  facet_wrap(vars(country_label)) +
  labs(
    title = "Up in the air: local or global?",
    subtitle = str_wrap("Heatmaps show monthly ratios of domestic to international flights across nine countries (2011-2019). Purple indicates more international flights, while green shows more domestic flights. Country labels display the average ratio during the entire 2011-2019 period - for example, '3 times more domestic' means that country had three domestic flights for every international one.", 130),
    caption = "Source: Lai S., Sorichetta A. and WorldPop (2020) · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "top",
    legend.key.width = unit(5, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    strip.text = element_markdown(size = 10),
    # panel.spacing.x = unit(-0.8, "lines"),
    axis.title = element_blank(),
    axis.text = element_text(family = f1),
    axis.ticks = element_line(lineend = "round", color = "#a7adb2"),
    axis.ticks.length = unit(0.05, "lines"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 5, 10)
  ) +
  theme_guide(
    bracket = element_line(colour = "#a7adb2"),
  )
  


