library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')

all_coal <- emissions %>% 
  filter(str_detect(tolower(commodity), "coal")) %>% 
  group_by(parent_entity, parent_type, year, production_unit) %>% 
  summarise(production_value = sum(production_value)) %>% 
  ungroup() %>% 
  mutate(still_coal = any(2022 %in% year), .by = parent_entity) %>% 
  filter(year >= 1940) %>%
  group_by(parent_entity) %>% 
  arrange(year) %>% 
  mutate(
    latest_value = production_value[which.max(year)],
    entity_x = min(year)
  ) %>% 
  ungroup() %>% 
  filter(latest_value >= 70) %>%
  filter(still_coal) %>% 
  mutate(
    parent_entity = str_remove(parent_entity, " \\(Coal\\)"),
    entity = fct_reorder(parent_entity, -latest_value)
    )
  
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(all_coal) +
  ggridges::geom_ridgeline_gradient(aes(x = year, height = production_value, y = parent_entity, group = parent_entity, fill = production_value), scale = 0.002, linewidth = 0.8) +
  # Labels
  ggtext::geom_richtext(data = . %>% filter(latest_value == production_value), aes(x = entity_x, y = entity, label = paste0("**", parent_entity, "**<br><span style = 'font-size:9.5pt'>**", scales::number(round(latest_value)), "**  mil. tonnes/yr</span><br>", "<span style = 'font-size:9pt'>*", str_to_sentence(parent_type), "*</span>")), nudge_x = -1, family = f1b, hjust = 1, vjust = 0.2, color = "black", lineheight = 1.1, fill = NA, label.color = NA) +
  scale_fill_gradient(high = alpha("grey10", 0.90), low = alpha("grey90", 0.9)) +
  scale_x_continuous(expand = expansion(0), limits = c(1928, 2022), breaks = c(1940, 1960, 1980, 2000, 2022)) +
  scale_y_discrete(expand = expansion(mult = 0.05)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Chinese state coal production accounts for the biggest share of global CO<sub>2</sub> coal emissions",
    subtitle = "CO<sub>2</sub> emissions by the top 9 entities for all coal commodities. Text shows 2022 values, rounded to the nearest million tonnes<br>per year. The top 9 entities are ranked based on 2022 values.",
    caption = "Source: Carbon Majors · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#A8ADB1", color = NA),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = ggtext::element_markdown(face = "bold", size = 15),
    plot.subtitle = ggtext::element_markdown(),
    plot.caption = element_text(margin = margin(30, 0, 0, 0))
  )
  
