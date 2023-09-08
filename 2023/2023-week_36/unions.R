library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

f1 <- "Outfit"
f2 <- "Canela Deck"
  
union_pp <- states %>% 
  filter(sector == "Public" | sector == "Private") %>%
  select(year, state, state_abbreviation, sector, p_members)

ggplot(union_pp) +
  geom_line(aes(x = year, y = p_members, color = sector)) +
  scale_x_continuous(breaks = c(1983, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_color_manual(values = c("#007BFF", "#FF7F00", "grey40"), limits = rev) +
  coord_cartesian(clip = "off") +
  facet_geo(vars(state_abbreviation)) +
  labs(
    title = "Union Membership in the U.S.",
    subtitle = "Percent of employed workers who are union members by state and sector, 1983–2022",
    caption = "Source: Union Membership and Coverage Database · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.06, 0.97),
    legend.title = element_blank(),
    legend.key.width = unit(2.8, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(color = "#675567", size = 8),
    axis.text.x = element_text(hjust = c(0, 1)),
    panel.background = element_rect(fill = "grey95", color = "grey60", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.1),
    strip.text = element_text(color = "#675567", face = "bold", margin = margin(2, 0, 2, 0)),
    plot.title = element_text(family = f2, face = "bold", size = 18),
    plot.subtitle = element_text(margin = margin(0, 0, 15, 0), color = "#675567"),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), color = "#675567"),
    plot.margin = margin(15, 10, 15, 10)
  )

record_polaroid()
