library(tidyverse)
library(ggcirclepack)
library(ggfittext)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

drob_funs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-09/drob_funs.csv')

drob <- drob_funs %>% 
  count(pkgs, funs, sort = TRUE) %>% 
  group_by(pkgs) %>% 
  summarise(
    n = sum(n),
    f = list(funs) %>% unlist() %>%  paste(collapse = " · ") # thin spaces
  ) %>% 
  mutate(pkgs = fct_reorder(pkgs, n))

drob_top <- drob_funs %>% 
  count(pkgs, sort = TRUE) %>% 
  filter(!str_detect(pkgs, "unknown")) %>% 
  slice_max(order_by = n, n = 5)

drob_n <- drob_funs %>% 
  add_count(pkgs, funs, name = "n_fun_used") %>% 
  distinct(pkgs, funs, n_fun_used) %>% 
  add_count(pkgs, name = "n_pkg_funs") %>%
  mutate(istop = if_else(pkgs %in% drob_top$pkgs, TRUE, FALSE)) %>% 
  mutate(label = paste0(funs, "\n", n_fun_used)) %>% 
  mutate(label = fct_reorder(label, -n_pkg_funs)) %>% 
  mutate(fill = if_else(istop, pkgs, NA))

f1 <- "Fantasque Sans Mono"
f2 <- "LINE Seed Sans"

pal <- rev(MetBrewer::met.brewer("Lakota"))

p <- ggplot(drob_n, aes(id = label, area = n_fun_used, fill = fill)) +
  geom_circlepack(color = "black", linewidth = 0.2) +
  geom_circlepack_text(aes(color = if_else(istop, "white", "grey60")), check_overlap = TRUE, family = f1, lineheight = 0.9) +
  scale_fill_manual(values = pal, breaks = c(drob_top$pkgs, NA), labels = c(drob_top$pkgs, "Other"), na.value = "grey98") +
  scale_color_identity() +
  coord_fixed() +
  labs(
    # title = "David Robinson's TidyTuesday Functions",
    # subtitle = str_wrap("Number of times each function was used in David’s explorations of TidyTuesday datasets. The top five packages are highlighted with color, with ggplot2 taking the first position, its functions appearing more than 4 500 times in David’s code.", 105),
    # caption = "Source: funspotr · Graphic: Georgios Karamanis",
    fill = "Package"
  ) +
  guides(
    size = "none",
    ) +
  theme_void(base_family = f2) +
  theme(
    # legend.position = "bottom",
    # legend.title = element_blank(),
    legend.key.height = unit(1.5, "lines"),
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(hjust = 0)
  )

t <- drob_n %>% 
  arrange(-n_fun_used) %>% 
  slice_max(order_by = n_fun_used, n = 25) %>% 
  mutate(istop = if_else(pkgs %in% drob_top$pkgs, TRUE, FALSE)) %>% 
  mutate(fill = if_else(istop, pkgs, NA)) %>% 
  mutate(funs = fct_reorder(funs, n_fun_used)) %>% 
  ggplot(aes(x = n_fun_used, y = funs, fill = fill, label = paste0(funs))) +
  geom_col(width = 0.8) +
  geom_bar_text(min.size = 3, family = f1, color = "white", hjust = 1) +
  geom_text(aes(label = n_fun_used), family = f1, color = "black", hjust = 0, nudge_x = 10) +
  scale_fill_manual(values = pal, breaks = c(drob_top$pkgs, NA), labels = c(drob_top$pkgs, "Other"), na.value = "grey98") +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Top 25 functions"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.subtitle = element_text(margin = margin(0, 0, 0, 0))
  )
  
p + t +
  plot_annotation(
    title = "David Robinson's TidyTuesday Functions",
    subtitle = str_wrap("Number of times each function was used in David’s explorations of TidyTuesday datasets. The top five packages are highlighted with color, with ggplot2 taking the first position, its functions appearing more than 4 500 times in David’s code.", 160),
    caption = "Source: funspotr · Graphic: Georgios Karamanis"
  ) +
  plot_layout(widths = c(2, 1)) &
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 18, family = f2),
    plot.subtitle = element_text(family = f2),
    plot.caption = element_text(hjust = 0, , family = f2)
  )
