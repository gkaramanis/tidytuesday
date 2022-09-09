gg_record(dir = "tidytuesday-temp", device = "png", width = 16, height = 10, units = "in", dpi = 320)

cols_yrs %>%  
  filter(!is.na(hex)) %>% 
  left_join(themes, by = c("theme_id" = "id")) %>% 
  select(year, theme_name = name, set_name = name.y, hex, num_parts) %>% 
  filter(str_detect(set_name, "Advent Cale")) %>% 
  count(set_name, year, num_parts, hex) %>% 
  group_by(set_name, year) %>% 
  mutate(
    total = sum(n),
    freq = n / sum(n)
  ) %>% 
  slice_max(order_by = freq, n = 3, with_ties = FALSE) %>% 
  mutate(
    col_y = row_number(),
    size = 1/row_number()
    ) %>%
  ungroup() %>% 
  ggplot(aes(x = year, y = as.numeric(factor(set_name)) + col_y / 4, group = set_name)) +
  geom_line() +
  geom_point(aes(fill = hex, size = size), shape = 21) +
  geom_text(aes(x = 1999, y = as.numeric(factor(set_name)), label = set_name), hjust = 1, size = 3, stat = "unique") +
  scale_fill_identity() +
  scale_size_continuous(range = c(2, 5)) +
  scale_y_reverse() +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(0, 0, 0, 230),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  )
  
