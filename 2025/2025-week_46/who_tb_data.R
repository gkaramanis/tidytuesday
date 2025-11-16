library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11.5, height = 8, units = "in", dpi = 320)

who_tb_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv')

tb <- who_tb_data |> 
  filter(year == max(year)) |> 
  mutate(label = str_remove(country, " \\(.+\\)")) |>
  group_by(g_whoregion) |>
  mutate(show_label = case_when(
    c_newinc_100k > quantile(c_newinc_100k, 0.9, na.rm = TRUE) | e_mort_100k > quantile(e_mort_100k, 0.9, na.rm = TRUE) | e_pop_num > quantile(e_pop_num, 0.95, na.rm = TRUE) ~ TRUE,
    TRUE ~ FALSE
  )) |> 
  ungroup()

f1 <- "IBM Plex Sans Condensed"
f2 <- "Sofia Sans Extra Condensed"

pal <- list(
  bg = "#F8F9F6",
  panel = "#F3F4F2",
  strip = "#D4C6B8",
  green3 = "#56B884"
)

ggplot(tb, aes(x = c_newinc_100k, y = e_mort_100k)) +
  geom_point(aes(size = e_pop_num), shape = 21, alpha = 0.8, stroke = 0.4, fill = pal$green3) +
  ggrepel::geom_text_repel(data = . %>% filter(show_label), aes(label = label), size = 4, segment.size = 0.1, family = f2, bg.color = "white", seed = 99) +
  scale_x_continuous(breaks = seq(0, 600, 100), minor_breaks = seq(0, 600, 50)) +
  scale_y_continuous(breaks = seq(0, 300, 100), minor_breaks = seq(0, 300, 50)) +
  scale_size_area(max_size = 7) +
  facet_wrap(vars(g_whoregion), scales = "free") +
  labs(
    title = "Tuberculosis case notification and mortality rates in 2023",
    subtitle = str_wrap("Case notification rate vs TB mortality rate by WHO region. Points represent countries, sized by population. Labeled countries are those with highest notification rate, mortality, or population. While overall global TB deaths declined by about 5%, the case notification rate increased very slightly compared to 2022. In 2023, the notification rate remained highest in the Western Pacific and African regions, which together accounted for nearly 70% of global cases. Mortality rates were disproportionately high in several African countries, showing ongoing challenges in access to timely diagnosis and treatment.", 158),
    caption = "Source: World Health Organization · Graphic: Georgios Karamanis",
    x = "Case notification rate (new and relapse cases and cases with unknown previous TB treatment history per 100 000 population per year)",
    y = "Mortality rate (TB-related deaths per 100 000 population per year)",
    size = "Population"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal$bg, color = NA),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = pal$strip, color = NA),
    panel.border = element_rect(color = pal$strip),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )
  
record_polaroid()
