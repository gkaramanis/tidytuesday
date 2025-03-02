library(tidyverse)
library(ggpattern)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

model_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/model_dat.csv')

infertility_care <- model_dat %>% 
  filter(outcome == "rates of accessing infertility care among women who reported infertility by respondent characteristics") %>% 
  mutate(
    compare = str_to_title(compare),
    compare = fct_reorder(compare, point),
    x = as.numeric(factor(compare))
  )

f1 <- "Montagu Slab 144pt"
f2 <- "Geist"

col <- "#5E3C99"
col2 <- "#99663C"
  
ggplot(infertility_care) +
  geom_hline(yintercept = 50, color = col2, linewidth = 1) +
  geom_col(aes(x = x, y = point), fill = col) +
  geom_rect_pattern(aes(xmin = x - 0.45, xmax = x + 0.45, ymin = lower, ymax = upper), fill = NA, pattern_fill = colorspace::lighten(col, 0.4), pattern_color = NA, pattern_spacing = 0.0075, pattern_density = 0.4) +
  shadowtext::geom_shadowtext(aes(x = x, y = point, label = paste0(compare, "\n", point, "%")), hjust = 1, nudge_y = -1, size = 8, fontface = "bold", family = f1, lineheight = 0.95, color = colorspace::lighten(col, 0.95), bg.colour = colorspace::darken(col, 0.9), bg.r = 0.07) +
  coord_flip() +
  labs(
    title = "Disparities in Infertility Care Access",
    subtitle = str_wrap("Only 59.6% of women with infertility seek medical help, with race significantly affecting access. Non-Hispanic black and Mexican/American women sought care less frequently than non-Hispanic Asian or white women. Patterned fill shows 95% confidence intervals.", 116),
    caption = str_wrap("Source: Kelley, A. S., Qin, Y., Marsh, E. E., & Dupree, J. M. (2019). Disparities in accessing infertility care in the United States: results from the National Health and Nutrition Examination Survey, 2013–16. Fertility and Sterility, 112(3), 562–568 · Graphic: Georgios Karamanis", 144)
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 12, margin = margin(6, 0, 10, 0)),
    plot.caption = element_text(size = 11, hjust = 0, , margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
