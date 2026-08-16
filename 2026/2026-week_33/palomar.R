library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8.5, units = "in", dpi = 320)

palomar_emission_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_emission_lines.csv')
palomar_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-11/palomar_survey.csv')

kauff  <- function(x) 0.61 / (x - 0.05) + 1.3
kewley <- function(x) 0.61 / (x - 0.47) + 1.19

col_point  <- "#1D3557"
col_kauff  <- "#E4A11B"
col_kewley <- "#6A4C93"
col_bg     <- "grey99"

annot <- tribble(
  ~x,     ~y,     ~label,                          ~color,      ~size, ~fontface,
  -0.9,   -0.6,   "Lit by\nyoung stars",       "grey20",   4,     "bold",
  -0.05,  -0.65,  "A mix\nof both",                "grey20",   4,     "bold",
  -0.1,    0.9,   "Lit by\na black hole",      "grey20",   4,     "bold",
  -0.05,  -1,     "Kauffmann 2003",                col_kauff,   3,     "italic",
   0.23,  -0.4,   "Kewley 2001",                   col_kewley,  3,     "italic"
)

f1 <- "Familjen Grotesk"

pp <- palomar_emission_lines |> 
  left_join(palomar_survey |> select(galaxy_name, hubble_type), by = "galaxy_name") |>
  drop_na(nii_6583, oiii_5007, h_beta) |>
  mutate(
    h = hubble_type |> str_remove_all("[\\[\\]() ]") |> str_remove("^R") |> str_remove("^d"),
    morph = case_when(
      str_detect(h, "^E")               ~ "Elliptical",
      str_detect(h, "^S(A|B|AB)?0|^L")  ~ "Lenticular",
      str_detect(h, "^S")               ~ "Spiral",
      str_detect(h, "^I")               ~ "Irregular",
      TRUE                              ~ "Unknown"
    ),
    morph = fct_relevel(morph, c("Elliptical", "Lenticular", "Spiral", "Irregular", "Unknown"))
  ) 
  
p <- ggplot(pp, aes(x = log10(nii_6583), y = log10(oiii_5007 / h_beta))) +
  geom_point(aes(shape = morph), alpha = 0.5, color = col_point, size = 2.5) +
  geom_function(fun = kauff, xlim = c(-1.1, -0.2), color = col_kauff, linewidth = 1) +
  geom_function(fun = kewley, xlim = c(-1.1, 0.12), color = col_kewley, linewidth = 1) +
  shadowtext::geom_shadowtext(data = annot, aes(x, y, label = label, color = color, size = size, fontface = fontface), inherit.aes = FALSE, bg.color = col_bg, family = f1, lineheight = 0.9) +
  scale_color_identity() +
  scale_size_identity() +
  scale_shape_manual(values = c(Elliptical = 19, Lenticular = 1, Spiral = 8, Irregular = 4, Unknown = 6)) +
  labs(
    title = "Stars or a black hole?",
    subtitle = str_wrap("The Palomar survey measured the glow from gas in the cores of more than 400 nearby galaxies. The color of that glow shows what heats the gas, whether young stars or a giant black hole pulling in matter. Galaxies lit by stars fall to the lower left, those lit by a black hole to the upper right, with a mixed group between them. The curved lines mark the boundaries, and the point shapes show each galaxy's type.", width = 109),
    x = "Nitrogen glow, compared with hydrogen {.grey · log( [N II] λ6583 / Hα )}",
    y = "Oxygen glow, compared with hydrogen {.grey · log( [O III] λ5007 / Hβ )}",
    caption = "Source: Palomar spectroscopic survey of nearby galaxy nuclei (Ho, Filippenko & Sargent) · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.13),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA, color = col_point, linewidth = 0.1),
    legend.text = element_text(margin = margin(l = 5, r = 33)),
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid = element_blank(),
    axis.title.x = marquee::element_marquee(colour = "grey30"),
    axis.title.y = marquee::element_marquee(colour = "grey30", angle = 90),
    axis.text = element_text(color = "grey50"),
    plot.title = element_text(face = "bold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey30", size = 11, lineheight = 1.1, margin = margin(t = 4, b = 12)),
    plot.caption = element_text(color = "grey50", size = 8, margin = margin(t = 12), hjust = 0),
    plot.caption.position = "plot",
    plot.margin = margin(20, 30, 15, 20)
  )

h <- ggplot(pp) +
  geom_bar(aes(x = morph), width = 0.5, fill = col_point) +
  scale_x_discrete(limits = rev) +
  coord_flip(expand = FALSE) +
  theme_void()

p + inset_element(h, left = 0.92, right = 0.98, bottom = 0.04,  top = 0.22)
