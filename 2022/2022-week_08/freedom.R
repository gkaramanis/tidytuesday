library(tidyverse)
library(camcorder)
library(geofacet)
library(ggforce)
library(MetBrewer)
library(patchwork)

gg_record(dir = "temp", device = "png", width = 13, height = 11, units = "in", dpi = 320)

freedom_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') 

freedom <- freedom_raw %>% 
  janitor::clean_names() %>% 
  mutate(
    clpr = cl * pr,
    country = case_when(
      country == "Congo" ~ "Republic of the Congo",
      country == "United Republic of Tanzania" ~ "Tanzania",
      str_detect(country, "Ivoire") ~ "Côte d'Ivoire",
      str_detect(country, "Principe") ~ "São Tomé and Principe",
      TRUE ~ country
    )
  )

f1 <- "Outfit"
f2 <- "Source Serif Pro"

pal <- rev(met.brewer("Greek")[1:5])
pal2 <- colorspace::desaturate(met.brewer("VanGogh3")[2:8], 0.2)

col_bg = met.brewer("VanGogh3")[1]
col_seg = pal2[4]

p1 <- ggplot(freedom %>% filter(region_name == "Africa"), aes(cl, pr)) +
  geom_segment(data = data.frame(x = 7:1, xend = 7:1, y = 1, yend = 7), aes(x =  x, xend = xend, y = y, yend = yend), size = 0.1, alpha = 0.15) +
  geom_segment(data = data.frame(x = 1, xend = 7, y = 7:1, yend = 7:1), aes(x =  x, xend = xend, y = y, yend = yend), size = 0.1, alpha = 0.15) +
  # Country names
  geom_text(aes(4, 3.5, label = str_wrap(country, 10)), lineheight = 0.8, size = 4, stat = "unique", alpha = 0.15, family = "DIN Condensed") +
  #
  geom_segment(data = data.frame(country = "Morocco"), aes(x = 7, xend = 7, y = 7, yend = 1), arrow = arrow(length = unit(0.08, "npc")), color = col_seg) +
  geom_segment(data = data.frame(country = "Morocco"), aes(x = 7, xend = 1, y = 7, yend = 7), arrow = arrow(length = unit(0.08, "npc")), color = col_seg) +
  #
  geom_segment(aes(x = 7, xend = 7, y = 7, yend = 1), color = col_seg, stat = "unique", size = 0.2) +
  geom_segment(aes(x = 7, xend = 1, y = 7, yend = 7), color = col_seg, stat = "unique", size = 0.2) +
  #
  geom_text(data = data.frame(country = "Morocco"), aes(7, 1, label = "Better political\nrights rating"), hjust = 1, size = 2.5, nudge_x = -1, family = f2, color = col_seg, lineheight = 0.85) +
  geom_text(data = data.frame(country = "Morocco"), aes(1, 7, label = "Better civil\nliberties rating"), vjust = 1, hjust = 1, size = 2.5, nudge_y = -0.75, family = f2, color = col_seg, lineheight = 0.85) +
  geom_bspline2(aes(group = country, color = year), size = 0.8) +
  geom_point(aes(color = ifelse(year == 2020, year, NA)), size = 1.5) +
  scale_color_gradientn(colors = pal, na.value = NA) +
  scale_x_reverse() +
  scale_y_reverse() +
  coord_fixed(clip = "off") +
  facet_geo(vars(country), grid = "africa_countries_grid1") +
  labs(
    subtitle = "Evolution of ratings by country",
    caption = "Source: Freedom House & United Nations · Graphic: Georgios Karamanis"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_bg, color = NA),
    # strip.text = element_text(family = f1),
    strip.text = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    # panel.grid = element_line(color = col_seg, size = 0.01),
    panel.grid = element_blank(),
    plot.margin = margin(21, 20, 0, 20),
    plot.subtitle = element_text(family = f2, size = 11, hjust = 0.5, margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0,), family = f2, size = 10),
    panel.spacing.y = unit(1, "lines")
  )

l <- ggplot() +
  geom_line(aes(x = 0, y = 1995:2020, color = 1995:2020), size = 2) +
  geom_text(aes(x = 0, y = seq(1995, 2020, 5), label = seq(1995, 2020, 5)), nudge_x = 0.08, family = f1, size = 3.5) +
  scale_y_reverse() +
  scale_color_gradientn(colors = pal, na.value = NA) +
  xlim(0, 0.15) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA)
  )

p2 <- freedom %>%
  filter(region_name == "Africa") %>% 
  pivot_longer(c("cl", "pr"), names_to = "metric", values_to = "rating") %>% 
  mutate(
    metric = case_when(
      metric == "cl" ~ "Civil liberties",
      metric == "pr" ~ "Political rights",
      TRUE ~ metric
    ) 
  ) %>% 
  ggplot() +
  geom_bar(aes(y = year, fill = as.factor(rating)), position = "fill", width = 0.85) +
  scale_y_reverse(breaks = seq(1995, 2020, 5)) +
  scale_fill_manual(values = pal2) +
  facet_wrap(vars(metric), ncol = 1) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom", title = NULL, reverse = TRUE)) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  labs(
    subtitle = "Distribution of ratings\nLower score means better rating" 
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(10, 0, 10, 0),
    legend.text = element_text(family = f2),
    plot.background = element_rect(fill = col_bg, color = NA),
    axis.text.y = element_text(family = f1, margin = margin(0, 4, 0, 5)),
    plot.margin = margin(15, 5, 0, 15),
    plot.subtitle = element_text(size = 11, lineheight = 1, family = f2, hjust = 0.5),
    strip.text = element_text(margin = margin(10, 0, 10, 0), family = f2, size = 11)
  )

p1_grob <- get_geofacet_grob(p1)

p2 + p1_grob +
  plot_layout(widths = c(1, 3)) +
  inset_element(l, 0.83, 0.025, 0.91, 0.3) +
  plot_annotation(
    title = "Evolution of political rights and civil liberties ratings, 1995-2020"
    ) &
  theme(
    plot.title = element_text(family = f2, face = "bold", size = 25, hjust = 0.5, margin = margin(10, 0, 0, 0)),
    plot.background = element_rect(fill = col_bg, colour = NA),
    plot.margin = margin(10, 5, 10, 5)
  )

# ggsave(paste0("temp/", Sys.Date(), "_", format(Sys.time(), " %H-%M-%OS3"), ".png"), dpi = 320, height = 18, width = 8)
