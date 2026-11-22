library(tidyverse)
library(camcorder)
library(ggh4x)
library(marquee)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11.5, height = 8, units = "in", dpi = 320)

cholesterol_mortality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-17/cholesterol_mortality.csv')

# One row per person, lipid and subgroup (everyone, sex, race, age band)
cm <- cholesterol_mortality |>
  filter(followup_months > 0) |>
  mutate(
    female = as.integer(sex == "Female"),
    sex = if_else(female == 1, "Women", "Men"),
    age_grp = as.character(cut(age, c(18, 40, 60, 70, 80, Inf), labels = c("18–39", "40–59", "60–69", "70–79", "80+"), right = FALSE)),
    all = "All"
  ) |>
  rename(
    "Total cholesterol" = total_chol, "HDL" = hdl, "LDL" = ldl, "Triglycerides" = triglycerides,
    "Non-HDL" = non_hdl, "Total/HDL" = tc_hdl_ratio, "LDL/HDL" = ldl_hdl_ratio
  ) |>
  pivot_longer(`Total cholesterol`:`LDL/HDL`, names_to = "lipid", names_transform = as_factor, values_to = "level", values_drop_na = TRUE) |>
  pivot_longer(c(all, sex, race, age_grp), values_to = "subgroup")

# One formulaic "paper": deaths per SD of a lipid, adjusted for age and sex
fit_paper <- function(data, outcome) {
  data$y <- data$died * (outcome == "All causes" | data$cause_of_death %in% outcome)
  if (sum(data$y) < 5) return(NULL)
  glm(y ~ scale(level) + age + female, family = poisson, offset = log(followup_months), data = data) |>
    broom::tidy() |>
    filter(term == "scale(level)") |>
    select(est = estimate, p = p.value)
}

papers <- cm |>
  nest(.by = c(subgroup, lipid)) |>
  expand_grid(outcome = c("All causes", unique(na.omit(cm$cause_of_death)))) |>
  mutate(fit = map2(data, outcome, fit_paper)) |>
  select(-data) |>
  unnest(fit) |>
  # Benjamini–Yekutieli, as in Suchak et al. 2025
  mutate(p_adj = p.adjust(p, "BY")) |>
  pivot_longer(c(p, p_adj), names_to = "test", values_to = "p_value") |>
  mutate(
    test = if_else(
      test == "p",
      str_glue('Each test individually: {sum(p_value < 0.05)} "discoveries"'),
      str_glue("Corrected for all {n()} tests: {sum(p_value < 0.05)} left")
    ),
    .by = test
  ) |>
  mutate(
    finding = case_when(p_value >= 0.05 ~ "none", est > 0 ~ "up", .default = "down"),
    test = fct_inorder(test),
    subgroup = fct_relevel(subgroup, "All", "Women", "Men"),
    outcome = fct_rev(fct_relevel(outcome, "All causes"))
  )

f1 <- "Host Grotesk"

ggplot(papers, aes(x = subgroup, y = outcome, fill = finding)) +
  geom_tile(color = "grey99", linewidth = 0.4) +
  scale_fill_manual(values = c(none = "grey88", up = "#D9822B", down = "#5B4B8A")) +
  facet_nested_wrap(
    vars(test, lipid), ncol = 7,
    labeller = labeller(lipid = label_wrap_gen(10)),
    strip = strip_nested(text_x = elem_list_text(face = "bold", size = c(12, 9), hjust = c(0, 0.5), vjust = c(0, 0)), by_layer_x = TRUE)
  ) +
  labs(
    title = "How to get 214 papers out of one dataset",
    subtitle = "Suchak et al. (2025, *PLOS Biology*) found that hundreds of formulaic papers now mine NHANES, the US National Health and Nutrition Examination Survey, for single associations. Each tile is one such test: one lipid measure, one cause of death, one group drawn from the 52,678 US adults in the data. **{#D9822B Orange}**: higher values, more deaths. **{#5B4B8A Purple}**: higher values, fewer deaths (p < 0.05 for both). **Gray**: no link. Blank: fewer than five deaths. Columns in each panel, left to right: everyone, women, men, five age bands (18–39 to 80+) and five race groups. The bottom row uses the correction from Suchak et al. (Benjamini–Yekutieli) for running 946 tests at once.",
    caption = "Source: NHANES 1999–2018, mortality follow-up to 2019 · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_size = 10, base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.6, "lines"),
    panel.spacing.y = unit(0.8, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_marquee(width = 1, lineheight = 1),
    plot.caption = element_text(color = "grey40", margin = margin(t = 12)),
    plot.margin = margin(20, 20, 15, 20)
  )
