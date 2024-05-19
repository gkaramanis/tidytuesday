library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

age_groups <- c(
  "<18",
  "18-24",
  "25-34",
  "35-44",
  "45-54",
  "55-64",
  ">65"
  )

cs <- coffee_survey %>% 
  mutate(
    favorite = str_replace(favorite, "Regular", "Reg."),
    age = fct_relevel(str_remove(age, " years old"), age_groups),
    favorite = str_remove(favorite, " \\(.+\\)")
    ) %>%
  count(gender, age, favorite)  %>% 
  filter(!is.na(age) & !is.na(gender)) %>%
  mutate(total_gender = sum(n, na.rm = TRUE), .by = gender) %>% 
  mutate(total_gender_age = sum(n, na.rm = TRUE), .by = c(gender, age)) %>% 
  filter(total_gender > 100) %>% 
  group_by(gender, age) %>% 
  arrange(-n) %>% 
  mutate(
    rank_gender_age = row_number(),
    pct = round(n / total_gender_age * 100),
    favorite_label = case_when(
      # between(rank_gender_age, 2, 3) ~ paste0(pct, "%"),
      rank_gender_age == 1 ~ paste0(favorite, " ", pct, "%")
      )
    ) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("Redon", direction = -1)
pal[8] <- colorspace::lighten(pal[8], 0.2)

bg_col <- "#C4A484"
col <- "#442f19"
coln <- "#e9d6c0"

ggplot(cs, aes(y = n, x = age, fill = favorite, label = scales::number(total_gender_age))) +
  geom_bar(position = "fill", stat = "identity", color = col, linewidth = 0.3, width = 0.85) +
  geom_text(aes(label = favorite_label), position = position_fill(vjust = 0.5), color = "white", alpha = 0.85, family = f1b, stat = "unique") +
  geom_segment(aes(y = 1.01, yend = 1.01 + total_gender_age/15000), linewidth = 6, color = coln) +
  geom_text(aes(y = 1.015), hjust = 0, stat = "unique", family = f1b, color = col) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), expand = expansion(add = 0.01), labels = scales::label_percent()) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  coord_flip(clip = "off") +
  facet_wrap(vars(paste0(gender, " (", scales::number(total_gender), ")")), ncol = 1) +
  labs(
    title = "Evolving tastes: How coffee preferences change with gender and age",
    subtitle = str_wrap("Coffee preferences differ by gender and age according to data from the Great American Coffee Taste Test. Percentages show the favorite coffee drink within each group, and sample sizes (n) are listed on the right.", 140),
    caption = "Source: Great American Coffee Taste Test · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_minimal(base_family = f1b) +
  theme(
    legend.position = c(0.455, -0.05),
    legend.text = element_text(color = col),
    legend.text.position = "bottom",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(0.8, "line"),
    plot.background = element_rect(fill = bg_col, color = NA),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = col),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 14, color = col),
    plot.title = element_text(family = f2, face = "bold", size = 16),
    plot.subtitle = element_text(color = col, lineheight = 1),
    plot.caption = element_text(color = col, hjust = 0.5, margin = margin(60, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
  
  