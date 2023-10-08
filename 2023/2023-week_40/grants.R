library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 9, units = "in", dpi = 320)

grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')

grant_cat <- grant_opportunity_details %>% 
  select(opportunity_id, starts_with("category"), -category_explanation) %>% 
  pivot_longer(2:last_col(), names_to = "category") %>% 
  filter(value)

grant_weekday <- grants %>% 
  select(opportunity_id, posted_date) %>% 
  mutate(weekday = wday(posted_date, label = TRUE)) %>% 
  left_join(grant_cat) %>% 
  filter(!is.na(category)) %>% 
  count(category, weekday) %>% 
  group_by(category) %>% 
  mutate(
    total = sum(n),
    p = n / total
  ) %>% 
  ungroup() %>% 
  filter(total >= 50) %>% 
  mutate(
    nudge_y = if_else(p > 0.03, -0.025, 0.025),
    color = if_else(p > 0.03, "white", "black"),
    category = str_to_sentence(str_replace(str_remove(category, "category_"), "_", " "))
    ) %>% 
  complete(category, weekday, fill = list(n = 0, p = 0))

f1 <- "Outfit"
f2 <- "Domine"

ggplot(grant_weekday) +
  geom_col(aes(x = weekday, y = p, fill = p), width = 0.85) +
  geom_text(aes(x = weekday, y = p + nudge_y, label = n, color = color), family = f1) +
  scale_color_identity() +
  scale_y_continuous(labels = scales::label_percent()) +
  MetBrewer::scale_fill_met_c("OKeeffe2") +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(category), scales = "free_x") +
  labs(
    title = "Which weekday do most grants open?",
    subtitle = str_wrap("Percentage of grants by opening day since 2004. The numbers in the bars show the total number of grants that opened each weekday. Only categories with at least 50 grants are shown.", 95),
    caption = "Source: grants.gov · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", margin = margin(20, 0, 5, 0), size = 12, color = "#267287", family = f2),
    panel.spacing.x = unit(1, "line"),
    axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
    plot.title = element_text(family = f2, size = 16, face = "bold", color = "#267287"),
    plot.subtitle = element_text(family = f2, size = 12),
    plot.caption = element_text(family = f2, color = "#267287", face = "bold", margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 20, 20, 20)
  )
  