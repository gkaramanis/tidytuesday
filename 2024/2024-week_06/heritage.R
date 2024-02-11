library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 16, height = 16, units = "in", dpi = 320)

heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')

her_tidy <- heritage %>% 
  pivot_longer(2:3, names_to = "year", values_to = "n")

col_sv <- "#4472D1"
col_dk <- "#DE5F4A"
col_no <- "#2A324E"
col_grey <- "#8F9498"
col_lightgrey <- "#EBEFEF"


f1 <- "Outfit"

t <- theme_void(base_family = f1) +
theme(
  axis.text = element_text(color = col_grey, size = 16),
  plot.margin = margin(10, 10, 10, 10),
  panel.grid.major = element_line(color = col_grey, linewidth = 0.15)
)


# Plot 1
p1 <- ggplot(her_tidy %>% mutate(country = fct_reorder(country, -n)), aes(year, n, fill = country, label = n)) +
  geom_bar(position = "stack", stat = "identity", width = 0.55) +
  geom_text(size = 6, family = f1, position = position_stack(vjust = 0.5), stat = "identity", color = "grey99") +
  geom_text(data = . %>% filter(year == 2022), aes(x = 3, label = country, color = country), size = 7, hjust = 0, family = f1, position = position_stack(vjust = 0.5), stat = "identity", fontface = "bold") +
  stat_summary(fun = sum, aes(label = after_stat(y), group = year), geom = "text", size = 6, vjust = -1, fontface = "bold") +
  scale_x_discrete(expand = expansion(mult = c(1, 0))) +
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 40)) +
  scale_color_manual(values = c(col_sv, col_dk, col_no)) +
  scale_fill_manual(values = c(col_sv, col_dk, col_no)) +
  coord_cartesian(clip = "off") +
  t +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(margin = margin(0, 15, 0, 0)),
    plot.margin = margin(40, 200, 40, 40)
  )


# Plot 2
y <- c(2.1, 2.1, 1.8, 1.8, 1.95, 1.95)

p2 <- ggplot() +
  annotate("text", x = 10, y = -1, label = "World Heritage\nSites", family = f1, size = 6, fontface = "bold", lineheight = 0.9) +
  geom_segment(aes(x = 0.5, xend = 19.5, y = 1.95, yend = 1.95), linewidth = 14, lineend = "round", color = col_lightgrey) +
  annotate("segment", x = seq(0, 20, 1), xend = seq(0, 20, 1), y = 1.6, yend = 2.2, color = "white") +
  annotate("text", x = seq(0, 20, 5), y = 1.4, label = seq(0, 20, 5), family = f1, size = 5, color = col_grey) +
  geom_segment(data = heritage, aes(x = `2004`, xend = `2022`, y = unique(y), yend = unique(y), color = country), linewidth = 1) +
  geom_point(data = her_tidy, aes(x = n, y = y, color = country), size = 8) +
  geom_text(data = her_tidy, aes(x = n, y = y, label = paste0("'", substr(year, 3, 4)), color = country), family = f1, color = "white", size = 3.5) +
  scale_x_continuous(limits = c(0, 40), breaks = c(8, 10, 15), labels = c("Norway", "Denmark", "Sweden")) +
  scale_y_continuous(limits = c(-1, 2.2), expand = c(0, 0)) +
  scale_color_manual(values = c(col_sv, col_dk, col_no), breaks = c("Sweden", "Denmark", "Norway"), expand = c(0, 0)) +
  geomtextpath::coord_curvedpolar(start = -pi/2, clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(color = c(col_no, col_dk, col_sv), face = "bold", size = 14)
  )

p2_t <- ggtrace::with_ggtrace(
  x = p2 + theme(aspect.ratio = 0.55),
  method = Layout$render,
  trace_steps = 5L,
  trace_expr = quote({
    panels <- lapply(panels, editGrob, vp = viewport(yscale = c(0.5, 1)))
  }),
  out = "g"
)


# Plot 3
p3 <- heritage %>% 
  mutate(
    change = round((`2022` - `2004`) / `2004` * 100),
    strip_label = paste0("▲\n", change, "%\n\n", country),
    ) %>% 
  pivot_longer(2:3, names_to = "year", values_to = "n") %>%
  mutate(year_label = paste0("'", substr(year, 3, 4))) %>% 
  mutate(strip_label = fct_inorder(strip_label)) %>% 
  ggplot(aes(year_label, n)) +
  geom_col(aes(fill = country, alpha = year == 2004, y = n), width = 0.1) +
  geom_point(color = "white", size = 8) +
  geom_point(aes(fill = country, alpha = year == 2004), size = 7, shape = 21, stroke = 0) +
  geom_text(aes(label = n), family = f1, nudge_y = 1, fontface = "bold", size = 7) +
  geom_segment(aes(x = 0.5, xend = 2.5, y = -Inf, yend = -Inf), color = col_grey, linewidth = 0.15) +
  facet_wrap(vars(strip_label), nrow = 1, strip.position = "bottom") +
  scale_color_manual(values = c(col_no, col_dk, col_sv), breaks = c("Norway","Denmark", "Sweden")) +
  scale_fill_manual(values = c(col_no, col_dk, col_sv), breaks = c("Norway","Denmark", "Sweden")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  scale_alpha_manual(values = c(1, 0.3)) +
  coord_cartesian(clip = "off") +
  t +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 15, margin = margin(10, 0, 20, 0), color = col_grey),
    strip.placement = "outside",
    strip.text = element_text(size = 16, face = "bold"),
    panel.spacing.x = unit(5, "lines"),
    plot.margin = margin(60, 40, 40, 40)
  )

p4 <- ggplot() +
  geom_text(aes(0, 0, label = "1 dataset\n3 visualizations"), family = f1, size = 15, fontface = "bold") +
  geom_text(aes(0, -5, label = "Source: UNESCO World Heritage Sites\nGraphic: Georgios Karamanis"), family = f1, size = 6, fontface = "bold", color = col_grey) +
  scale_y_continuous(limits = c(-8, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
  

# Combine plots
(p1 + p2_t) / (p3 + p4)
