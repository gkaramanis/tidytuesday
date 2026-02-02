library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/companies.csv')

co_qs <- companies |> 
  count(owner_qualification, company_size) |> 
  group_by(owner_qualification) |>
  mutate(
    owner_n = sum(n),
    ratio = n / owner_n
  ) |>
  ungroup() |> 
  filter(owner_n > 1) |>
  # Calculate micro-enterprise ratio for sorting owner_qualification
  group_by(owner_qualification) |>
  mutate(
    micro_ratio = sum(ratio[company_size == "micro-enterprise"]),
    company_size = fct_relevel(
      company_size, 
      "micro-enterprise", "small-enterprise", "other"
    )
  ) |>
  ungroup() |>
  mutate(owner_qualification = fct_reorder(owner_qualification, micro_ratio, .desc = FALSE))

positions <- co_qs |>
  group_by(owner_qualification) |>
  summarise(total_n = sum(n), .groups = "drop") |>
  mutate(width = log10(total_n)) |>
  mutate(x = cumsum(width) - width/2)

co_qs_pos <- co_qs |>
  left_join(positions, by = "owner_qualification")

f1 <- "DIN Condensed"
f2 <- "Iosevka"

pal <- MetBrewer::met.brewer("Juarez")[2:4]

ggplot(co_qs_pos, aes(x, n, fill = company_size, width = width), ) +
  geom_col(position = position_fill(), alpha = 1, color = "white") +
  shadowtext::geom_shadowtext(aes(label = scales::number(n), bg.colour = after_scale(fill)), position = position_fill(vjust = 0.5), size = 5, family = f2, fontface = "bold") +
  geom_text(data = positions, aes(x = x, y = -0.01, label = owner_qualification), inherit.aes = FALSE, hjust = 1, family = f1) +
  scale_fill_manual(values = pal, name = "Company size", guide = guide_legend(label.position = "left")) +
  coord_flip(clip = "off") +
  labs(
    title = "Brazilian companies by owner qualification and size",
    subtitle = str_wrap("Distribution of micro-enterprises, small-enterprises, and other company sizes across different owner qualifications. Bar width represents the log-scaled total number of companies in each owner type. Owner types are ordered by the proportion of micro-enterprises they represent.", 100),
    caption = "Source: Cadastro Nacional da Pessoa Jurídica (CNPJ) · Graphic: Georgios Karamanis") +
  theme_void(base_family = f2) +
  theme(
    legend.position = c(0.85, 1.08),
    legend.title = element_text(size = 14, face = "bold", family = f1, hjust = 1),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 20, face = "bold", family = f1, margin = margin(0, 0, 5, -170)),
    plot.subtitle = element_text(size = 12, margin = margin(0, 0, 15, -170)),
    plot.caption = element_text(face = "bold"),
    plot.margin = margin(15, 10, 10, 200)
  )

record_polaroid()
