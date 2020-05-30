dark_rum <- cocktails %>% mutate_at(vars(starts_with("ingredient")), tolower) %>%
  mutate(glass = tolower(glass)) %>% 
  filter(ingredient1 == "dark rum" & glass != "white wine glass") %>% 
  pivot_longer(measure1:measure9, names_to = "measure_n", values_to = "measure") %>% 
  mutate(
    measure_dec = case_when(
    str_detect(measure, "1/2") ~ str_replace(measure, "\\s*1/2", ".5"),
    str_detect(measure, "1/8") ~ str_replace(measure, "\\s*1/8", ".25"),
    str_detect(measure, "1-2") ~ str_replace(measure, "1-2", "1.5"),
    TRUE ~ measure
    ),
    ing_v = case_when(
      str_detect(measure_dec, "oz") ~ as.numeric(str_extract(measure_dec, "\\d*\\.?\\d*")) * 29.6
    )
    ) %>%
  left_join(glasses) %>% 
  rowwise() %>% 
  mutate(
    ing_h1 = 3 * ing_v / (pi * (r_top ^ 2 + r_bottom ^ 2 + (r_top * r_bottom))),
    ing_r1 = ing_h1 / tan(th) + r_bottom,
    ing_x1 = list(c(-ing_r1, ing_r1, ing_r1, -ing_r1)),
    ing_y1 = list(c(ing_h1, ing_h1, 0, 0)),
  ) %>% 
  filter(measure_n == "measure1") %>% 
  unnest(c(x_inner, x_outer, y_inner, y_outer, ing_x1, ing_y1))

