library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 14, height = 6, units = "in", dpi = 320)

basotho_wool <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-04/basotho_wool.csv')

f1 <- "Graphik"
f2 <- "Iosevka Charon"

wool <- basotho_wool |>
  filter(cmd_code == 5101) |>
  filter(ref_year > 2015) |>
  filter(reporter_desc %in% c("China", "South Africa")) |>
  mutate(date = ymd(paste(ref_year, ref_month, "01", sep = "-"))) |>
  mutate(
    total = sum(primary_value, na.rm = TRUE),
    share = primary_value / total,
    .by = date
  ) |>
  complete(reporter_desc, date = seq(min(date), max(date), by = "month")) |>
  mutate(
    ref_year = year(date),
    ref_month = month(date)
  )

# Copy each December and January to make lines continuous across facets
wool_line <- wool |>
  bind_rows(
    filter(wool, ref_month == 12) |> mutate(ref_year = ref_year + 1, ref_month = 0),
    filter(wool, ref_month == 1) |> mutate(ref_year = ref_year - 1, ref_month = 13)
  ) |>
  filter(ref_year %in% unique(wool$ref_year))

ggplot(wool_line, aes(x = ref_month, y = primary_value, color = reporter_desc, group = reporter_desc)) +
  geom_rect(data = tibble(ref_year = c(2018, 2019), xmin = c(5, -Inf), xmax = c(Inf, 9)), aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "grey94", inherit.aes = FALSE) +
  geom_line(linewidth = 1.2, lineend = "round") +
  scale_x_continuous(breaks = c(1, 4, 7, 10), labels = month.abb[c(1, 4, 7, 10)], expand = expansion(0)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_long_scale())) +
  scale_color_manual(values = c("China" = "#AD2B10", "South Africa" = "#FFB81C")) +
  coord_cartesian(xlim = c(0.5, 12.5)) +
  facet_wrap(vars(ref_year), nrow = 1) +
  labs(
    title = "Lesotho's wool, rerouted",
    subtitle = str_wrap("Monthly value of wool imported from Lesotho, by importing country. In the shaded months, May 2018 to September 2019, Lesotho required all wool to be sold through one licensed broker at home, the Chinese-owned Lesotho Wool Centre, ending decades of auctioning at Port Elizabeth. China's share went from 56% before to 94% during, and 33% after. Farmers went unpaid, protested, and the rules changed in 2019. Values are importer-reported and wool through South Africa is largely re-exported, so this shows the route, not the final buyer. Raw wool only, no mohair. Breaks in the lines are months with no recorded trade.", 162),
    caption = "Source: UN Comtrade via the comtradr R package · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(family = f1, size = 10),
    legend.key.spacing = unit(1, "lines"),
    axis.title = element_blank(),
    axis.text.x = element_text(color = c("black", rep("grey70", 3)), face = c("bold", rep("plain", 3))),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0, "pt"),
    plot.title = element_text(family = f1, size = 18, face = "bold"),
    plot.subtitle = element_text(family = f1, size = 12, margin = margin(5, 0, 15, 0)),
    plot.caption = element_text(family = f1, size = 9, hjust = 0, margin = margin(15, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
