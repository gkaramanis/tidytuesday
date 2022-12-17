library(tidyverse)
library(ggrepel)
library(camcorder)
library(ggtext)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 9, units = "in", dpi = 320)

state_retail <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc")

coverage_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/coverage_codes.csv')

ret_us <- state_retail %>% 
  filter(state_abbr == "USA") %>% 
  filter(subsector != "total") %>%
  filter(month == 3) %>%
  select(year, subsector, state_abbr, change_yoy, change_yoy_se) %>% 
  mutate(
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se)
    ) %>% 
  group_by(subsector) %>% 
  mutate(higher = if_else(change_yoy < lead(change_yoy), TRUE, FALSE))

ret_labs <- ret_us %>% 
  filter(year == 2022)

f1 <- "Outfit"
f2 <- "Newsreader text"

ggplot(ret_us) +
  geom_point(aes(x = year, y = change_yoy, size = change_yoy_se), color = "#186918", alpha = 0.15) +
  geom_line(aes(x = year, y = change_yoy, group = subsector, color = higher), lineend = "round", show.legend = FALSE, linewidth = 0.7) +
  geom_label_repel(data = ret_labs, aes(x = 2022.01, y = change_yoy, label = subsector), direction = "y", hjust = 0, nudge_x = 0.2, family = f1, fontface = "bold", segment.size = 0.1, label.r = 0.5, label.padding = 0.35, label.size = 0, seed = 999, size = 3.5, segment.color = "grey60") +
  annotate("richtext", x = 2020, y = 55, hjust = 0.5, vjust = 1, label = "Most subsectors suffered<br>a <span style='color:#EBBC55'>**drop**</span> during the<br>novel coronavirus outbreak<br>in March 2020<br>↓", family = f1, label.r = unit(0.5, "lines"), label.color = "grey80", label.size = 0.1, lineheight = 1.3) +
  annotate("richtext", x = 2021, y = -45, hjust = 0.5, vjust = 0, label = "↑<br>Almost all subsectors<br>recorded a year-over-year<br><span style='color:#691869'>**increase**</span> in March 2021<br>compared to March 2020", family = f1, label.r = unit(0.5, "lines"), label.color = "grey80", label.size = 0.1, lineheight = 1.3) +
  scale_x_continuous(limits = c(2019, 2023.1), breaks = 2019:2022) +
  scale_color_manual(values = c("#EBBC55", "#691869")) +
  scale_radius(name = "Standard error for\nyear-over-year\npercentage change") +
  labs(
    title = "Year-over-year percentage changes in the month of March",
    subtitle = "For total sales in all retail subsectors in the US, 2019-2022",
    caption = "Source: United States Census Bureau · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.81, 0.825),
    legend.title = element_text(colour = "grey40"),
    legend.text = element_text(colour = "grey40"),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(size = 24, family = f2, face = "bold"),
    plot.subtitle = element_text(size = 18, family = f2),
    plot.caption = element_text(family = f2, margin = margin(20, 0, 0, 0)),
    plot.margin = margin(15, 20, 10, 20)
  )
