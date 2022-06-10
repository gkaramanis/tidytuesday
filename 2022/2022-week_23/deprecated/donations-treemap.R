library(tidyverse)
library(camcorder)
library(treemapify)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')

fortune_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv')

pride_fortune <- pride_aggregates %>% 
  full_join(fortune_aggregates, by = "Company") %>% 
  janitor::clean_names() %>% 
  select(company, total_pride = total_contributed_x, total_fortune = total_contributed_y) %>% 
  mutate(
    total_fortune = replace_na(total_fortune, 0),
    total_pride = replace_na(total_pride, 0),
    company = fct_reorder(company, total_fortune)
    ) %>% 
  arrange(-total_fortune) %>% 
  filter(company != "Grand Total") %>% 
  mutate(
    total = if_else(total_fortune >= 50000, scales::unit_format(unit = "K", scale = 1e-3, accuracy = 1)(total_fortune), ""),
    label = str_replace_all(company, " ", "\n"),
    label = if_else(total_fortune >= 50000, paste0(label, "\n", total), label),
    pride = if_else(total_pride > 0, "Pride Sponsors", "Not Pride Sponsors")
    ) 


f1 <- "Outfit"
f2 <- "Borsok"

col1 <- "#E69A8D"
col2 <- "#6E5B9A"
  
ggplot(pride_fortune, aes(area = total_fortune, fill = pride, label = label, subgroup = pride)) +
  geom_treemap(aes(color = after_scale(colorspace::darken(fill, 0.25))), start = "topright", size = 1) +
  geom_treemap_text(aes(color = after_scale(colorspace::lighten(fill, 0.7))), place = "center", family = f1, grow = FALSE, start = "topright", min.size = 1) +
  geom_treemap_subgroup_text(start = "topright", place = "top", size = 15, family = f2, color = "grey97") +
  labs(
    title = "Fortune 500 Companies Donations to Anti-LGBTQ Campaigns",
    caption = "Source: Data For Progress · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  scale_fill_manual(values = c(col1, col2)) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(size = 20, hjust = 0.5, family = f1, margin = margin(10, 0, 10, 0)),
    plot.caption = element_text(size = 8, hjust = 0.98, color = "grey50", family = f1, margin = margin(5, 0, 5, 0))
  )
  