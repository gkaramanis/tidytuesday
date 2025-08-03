library(ggiraph)

p1a <- ggplot(top_movies, aes(views, title, data_id = title, tooltip = title)) +
  geom_col_interactive(color = col_red, fill = NA, linewidth = 1, width = 0.85) +
  # geom_shadowtext(aes(x = 2e6, label = title), hjust = 0, vjust = -0.4, color = col_red, family = f1, size = 3.5, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  # geom_shadowtext(aes(x = views - 2e6, label = scales::number(janitor::round_half_up(views, -6), scale_cut = scales::cut_short_scale())), hjust = 1, color = col_red, family = f1, fontface = "bold", bg.colour = alpha("white", 0.5)) 
  scale_x_continuous(expand = expansion(mult = c(0.095, 0.155))) +
  scale_y_discrete(expand = expansion(mult = 0.175)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  inset_element(top_movies_img, 0, 0, 1, 1, on_top = FALSE)

p1b <- ggplot(top_shows, aes(views, title, data_id = title, tooltip = title)) +
  geom_col_interactive(color = col_red, fill = NA, linewidth = 1, width = 0.85) +
  # geom_shadowtext(aes(x = 2e6, label = title), hjust = 0, vjust = -0.4, color = col_red, family = f1, size = 3.5, fontface = "bold", bg.colour = alpha("white", 0.5)) +
  # geom_shadowtext(aes(x = views - 2e6, label = scales::number(janitor::round_half_up(views, -6), scale_cut = scales::cut_short_scale())), hjust = 1, color = col_red, family = f1, fontface = "bold", bg.colour = alpha("white", 0.5)) 
  scale_x_continuous(expand = expansion(mult = c(0.095, 0.155))) +
  scale_y_discrete(expand = expansion(mult = 0.175)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, color = NA)
  ) +
  inset_element(top_shows_img, 0, 0, 1, 1, on_top = FALSE)


p2 <- ggplot(ms_data, aes(views, px, color = type, fill = type, data_id = title, tooltip = title)) +
  geom_line(aes(y = expected_px), linetype = "dashed") +
  geom_point_interactive(size = 3, shape = 21, alpha = 0.8, stroke = 0.7) +
  # geom_text(data = . %>% filter(px > 600), aes(label = title), nudge_x = -5e6, hjust = 1, family = f1, size = 3, check_overlap = TRUE) +
  # geom_text(data = . %>% filter(px < 600), aes(label = title), nudge_x = 5e6, hjust = 0, family = f1, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_color_manual(values = c("Movies" = "#8482F7", "Shows" = "#62D6BD")) +
  scale_fill_manual(values = c("Movies" = "#8482F7", "Shows" = "#62D6BD")) +
  facet_wrap(vars(type), ncol = 2) +
  labs(
    subtitle = str_wrap("Measured bar lengths in Netflix report charts. The line shows their expected lengths based on the views of the top movie or show", width = 100),
    x = "Views",
    y = "pixels"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "grey19", color = NA),
    panel.grid = element_blank()
  )

f <- 20

ppp <- (p1a + p1b) / p2 +
  plot_layout(heights = unit(c(dim_w / f, dim_w / f), "points"))

ppp
multiplot <- girafe(ggobj = ppp)
multiplot
htmltools::save_html(multiplot, "~/Desktop/ggiraph.html")
