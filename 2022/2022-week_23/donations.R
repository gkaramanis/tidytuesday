library(tidyverse)
library(camcorder)
library(ggsvg)
library(shadowtext)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')

top6 <- pride_aggregates %>% 
  janitor::clean_names() %>% 
  rename("total" = "total_contributed") %>% 
  filter(company != "Grand Total") %>% 
  slice_max(order_by = total, n = 6) %>% 
  mutate(
    company = fct_reorder(company, total),
    color = c("#e40303",
                 "#ff8c00",
                 "#ffed00",
                 "#008026",
                 "#004dff",
                 "#750787"),
    logo_size = c(1, 0.65, 1.8, 1.2, 1, 1.2),
    nudge_logo = c(1.1, 1, 2, 1.3, 1.1, 1.3),
    label = scales::unit_format(unit = "K", scale = 1e-3, accuracy = 1)(total),
    nudge_label = c(-8e4, rep(10e3, 5)),
    nudge_dots = c(-31e4, 180e3, 158e3, rep(150e3, 3))
  ) %>% 
  rowwise() %>% 
  mutate(
    path = paste0(here::here("2022/2022-week_23/logos/"), company, ".svg"),
    logo = paste(readLines(path), collapse = "\n"),
    politicians = paste(rep("·", number_of_politicians_contributed_to), collapse = ""),
    politicians = str_replace_all(politicians, "··········", "··········\n")
    ) %>% 
  ungroup()


f1 <- "Outfit"
f2 <- "Plus Jakarta Sans"


ggplot(top6, aes(x = total, y = company)) +
  geom_col(aes(x = 650000, fill = color), width = 0.999) +
  geom_col(fill = "grey90", color = "grey90", size = 0.1, width = 1) +
  geom_point_svg(aes(x = total - 30000 * nudge_logo, svg = logo, size = 20 * logo_size)) +
  geom_shadowtext(aes(x = total + nudge_label,
                label = label,
                hjust = if_else(company == "Toyota", 1, 0),
                bg.color = if_else(company == "Toyota", "grey50", colorspace::darken(color, 0.4)),
                ), size = 20, family = f2, fontface = "bold", color = "white", bg.r = 0.06) +
  geom_shadowtext(aes(x = total + nudge_dots,
                      label = politicians,
                      bg.color = if_else(company == "Toyota", "grey50", colorspace::darken(color, 0.4))
                      ), lineheight = 0.18, hjust = 0, vjust = 1, nudge_y = 0.46, size = 12, bg.r = 0.05) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  labs(
    title = "Pride Sponsors Donations to Anti-LGBTQ Campaigns",
    subtitle = "30 corporate Pride sponsors have given $1.6 million to 92 anti-LGBTQ politicians. Here are shown the\n6 companies with the biggest donations. The dots show the number of politicians they have donated to.",
    caption = "Source: Data For Progress · Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.title = element_text(size = 28, family = f1, face = "bold"),
    plot.subtitle = element_text(size = 15, margin = margin(10, 0, 15, 0), lineheight = 1),
    plot.caption = element_text(size = 10, color = "grey30", family = f1, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )
  
