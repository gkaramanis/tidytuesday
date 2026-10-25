library(tidyverse)
library(ggridges)
library(ggfx)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

ai_salary_majors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-20/ai_salary_majors.csv')
# major_occupations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-20/major_occupations.csv')


top_majors <- ai_salary_majors |> 
  group_by(broad_field_short) |>
  mutate(broad_n_grad = sum(n_graduates)) |>
  ungroup() |>
  filter(dense_rank(desc(broad_n_grad)) <= 8) |> 
  mutate(broad_field_short = fct_reorder(broad_field_short, ai_exposure)) |>
  select(broad_field_short, ai_exposure_image, ai_exposure_language) |>
  pivot_longer(cols = c(ai_exposure_image, ai_exposure_language), names_to = "ai_exposure", values_to = "value")
  
f1 <- "Inter Display"
f2 <- "Inter Display Black"

systemfonts::register_variant(name = f2, family = "Inter Display", weight = "heavy")

ridge_layer <- function(var) {
  geom_density_ridges(
    data = \(d) filter(d, ai_exposure == var),
    jittered_points = TRUE, position = position_points_jitter(width = 0.01, height = 0), rel_min_height = 0.005,
    point_shape = "|", point_size = 4, scale = 1, alpha = 1
  )
}

ggplot(top_majors, aes(x = value, y = broad_field_short, fill = ai_exposure, color = after_scale(colorspace::darken(fill, 0.6)))) +
  geom_vline(xintercept = 0, color = "#5E5A73", linetype = "dotted") +
  geom_text(aes(label = broad_field_short, x = -Inf), hjust = 0, vjust = 0, size = 20, fontface = "bold", color = "#3B3750", alpha = 0.1, stat = "unique") +
  as_reference(ridge_layer("ai_exposure_image"), id = "image") +
  with_blend(ridge_layer("ai_exposure_language"), bg_layer = "image", blend_type = "multiply") +
  scale_fill_manual(values = c("#EE9BB8", "#9AD9E6")) +
  scale_x_continuous(expand = expansion(mult = c(0.005, 0.01))) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "How exposed are college majors to generative AI?",
    subtitle = "Each curve shows the majors within one of the 8 broad fields with the most graduates, scored by how much their graduates' occupations overlap with {#D0608F **image generation**} and {#2E9BB5 **language modeling**}. A score of 0 is the average occupation. Higher means more overlap, not that the job will be automated.",
    caption = "Source:  AI Occupational Exposure index (2023) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FCFBFD", color = NA),
    panel.grid.major = element_line(color = "#ECE9F1"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 14, color = "#5E5A73", margin = margin(t = 10)),
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, size = 28, color = "#3B3750", margin = margin(b = 5)),
    plot.subtitle = marquee::element_marquee(size = 13, color = "#5E5A73", lineheight = 1, width = 0.95, margin = margin(t = 0, b = 10)),
    plot.margin = margin(15, 15, 15, 15)
  )
