library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

wwbi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_data.csv')

wwbi_series <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_series.csv')

wwbi_country <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_country.csv')

# Idea 
#   https://www.cgdev.org/blog/three-lessons-world-banks-new-worldwide-bureaucracy-indicators-database

income_gr_order <- c("Low", "Lower middle", "Upper middle", "High") 

wwbi_pub <- wwbi_data %>% 
  filter(indicator_code == "BI.EMP.TOTL.PB.ZS" | indicator_code == "BI.WAG.TOTL.PB.ZS") %>% 
  group_by(country_code, indicator_code) %>% 
  filter(year == max(year, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(wwbi_series) %>% 
  left_join(wwbi_country) %>% 
  filter(!is.na(income_group)) %>% 
  mutate(
    income_gr = str_remove(income_group, " income"),
    income_gr = fct_relevel(income_gr, income_gr_order),
    value = if_else(indicator_code == "BI.EMP.TOTL.PB.ZS", value * 100, value)
    ) %>% 
  group_by(income_gr, indicator_name) %>% 
  mutate(highlight = (value >= max(value) * 0.92) | (value <= min(value) * 1.2)) %>% 
  ungroup() %>% 
  mutate(
    indicator_name = case_when(
      indicator_name == "Public sector employment, as a share of total employment" ~ "Public sector employment, as a percentage of total employment",
      indicator_name == "Wage bill as a percentage of Public Expenditure" ~ "Wage bill, as a percentage of public expenditure",
    )
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(wwbi_pub, aes(x = value, y = income_gr)) +
  ggdist::geom_weave(aes(fill = year), color = "purple4", size = 0.3) +
  ggdist::stat_interval(aes(x = value, y = as.numeric(income_gr) - 0.3), height = 0, linewidth = 2.8, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = . %>% filter(highlight), aes(label = str_wrap(short_name, 12)), family = f1b, lineheight = 0.9, seed = 999, segment.size = 0.1, size = 3.5, direction = "y") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(labels = scales::wrap_format(10)) +
  scale_color_brewer(palette = "Oranges") +
  colorspace::scale_fill_binned_sequential(palette = "Purples", guide = guide_colorsteps(show.limits = TRUE, title = "")) +
  facet_wrap(vars(indicator_name), ncol = 1) +
  labs(
    title = "Public sector size tied to income",
    subtitle = str_wrap("The data suggests that the size of the public sector is indeed linked to income level, despite significant variations among countries within income groups. The chart shows the most recent data available for each country, covering the period from 2001 to 2020. The orange-colored lines show where the 50%, 80%, and 95% of the values in each group fall.", 135),
    caption = "Source: Worldwide Bureaucracy Indicators (World Bank) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1b) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2.5, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 12, color = "grey10"),
    strip.text = element_text(size = 15, face = "bold"),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = f1, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
