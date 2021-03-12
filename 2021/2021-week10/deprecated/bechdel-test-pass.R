bechdel_2020 <- bechdel %>% 
  filter(year %in% c(2020, 2015, 2010, 2005, 2000, 1995)) %>% 
  group_by(year) %>% 
  arrange(rating) %>% 
  mutate(
    # title = fct_reorder(title, -rating),
    rank = row_number(),
    hjust = (3 - rating)/3,
    nudge_x = case_when(
      rating == 3 ~ 0.15,
      rating == 0 ~ -0.15,
      TRUE ~ 0
    ),
    color = if_else(rating == 3, "darkgreen", "darkred")
    ) %>% 
  ungroup()

ggplot(bechdel_2020) +
  geom_vline(xintercept = 0, size = 0.1) +
  geom_text(aes(x = nudge_x, y = rank, label = title, hjust = hjust, color = color), size = 1) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_reverse() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(year), nrow = 1) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("temp", paste0("bechdel-test-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 12, width = 12)
