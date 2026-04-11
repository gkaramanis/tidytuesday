library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

repairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv')
# repairs_text <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs_text.csv')

repair_data <- repairs |> 
  filter(!is.na(category) & category != "Other" & category != "Furniture") |> 
  mutate(age_at_repair = year(repair_date) - estimated_year_of_production) |> 
  filter(between(age_at_repair, 0, 100)) |> 
  mutate(
    subcategory = case_when(
      str_detect(category, " non-electric") ~ "non-electric",
      str_detect(category, " electric") ~ "electric",
      category == "Textile" ~ "non-electric",
      category == "Jewelry" ~ "non-electric",
      TRUE ~ NA
    ),
    category = str_remove(category, " non-electric| electric")
  )

f1 <- "Karst"
f2 <- "Onest"

c_electric <- "#E69F00"
c_nonelectric <- "#009E73"
c_mixed <- "#CC79A7"
c_bg <- "#F8F5EF"
c_grid <- "#DDD9D0"
c_text <- "#4D4844"

ggplot(repair_data, aes(age_at_repair, repairability, group = interaction(category, subcategory), color = subcategory, fill = after_scale(color))) +
  geom_smooth(linewidth = 0.75, alpha = 0.1) +
  gghighlight::gghighlight(unhighlighted_params = list(se = FALSE, linewidth = 0.25)) +
  scale_x_reverse(labels = c(0, "·", 50, "·", 100)) +
  scale_color_manual(values = c("non-electric" = c_nonelectric, "electric" = c_electric), na.value = c_mixed) +
  facet_wrap(vars(category)) +
  theme_minimal(base_family = f2) +
  labs(
    title = "Are older things harder to fix?",
    subtitle = " At Repair Cafés worldwide, volunteer fixers help people repair broken household items since 2015. The smoothed lines show repairability scores by item age at the time of repair, with older items on the left. {.#E69F00 **Electric**} and {.#009E73 **non-electric**} types shown separately. For appliances and toys, scores show a crossover between the two types across the age range.",
    caption = "Source: RepairMonitor · Graphic: Georgios Karamanis",
    x = "Age at repair (years)",
    y = "Repairability"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = c_bg, color = NA),
    panel.grid = element_line(color = c_grid, linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_text(size = 10, color = c_text),
    axis.text = element_text(size = 8, color = c_text),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    strip.text = element_text(family = f2, face = "bold"),
    plot.title = element_text(family = f1, face = "bold"),
    plot.subtitle = marquee::element_marquee(width = 0.95, margin = margin(t = 0, b = 15)),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()

