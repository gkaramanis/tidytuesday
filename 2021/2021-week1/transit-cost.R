library(tidyverse)
library(countrycode)
library(cowplot)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_timeline <- transit_cost %>% 
  filter(!is.na(e)) %>% 
  group_by(country) %>% 
  mutate(
    continent = countrycode(country, origin = "ecb", destination = "continent"),
    country_name = countrycode(country, origin = "ecb", destination = "country.name"),
    tunnel_per = parse_number(tunnel_per) / 100,
    start_year = as.numeric(start_year),
    end_year = as.numeric(end_year),
    y_alt = rep(c(-1, 1), length.out = n())
  ) %>% 
  ungroup() %>% 
  group_by(country, y_alt) %>% 
  arrange(start_year) %>% 
  mutate(
    n_off = 1:n(),
    y_off = 0.9 * n_off
  ) %>% 
  ungroup() %>% 
  filter(continent == "Europe") %>% 
  mutate(country = fct_rev(country)) %>% 
  group_by(country) %>% 
  mutate(country_n = 5 * cur_group_id()) %>% 
  ungroup() %>% 
  group_by(country, city) %>% 
  mutate(city_n = 1:n()) %>% 
  ungroup()

transit_gr <- transit_timeline %>% 
  filter(country == "GR")

transit_se <- transit_timeline %>% 
  filter(country == "SE") %>% 
  mutate(city = if_else(city == "Malmo", "Malmö", city))

# fonts
f1 = "Atkinson Hyperlegible"
f1b = "Atkinson Hyperlegible Bold"
f2 = "Produkt"

# Common layers
gg_common <- list(
  # 2020 line
  geom_vline(xintercept = 2020, color = "grey"),
  # country label
  geom_text(aes(x = 2005, y = 1.5, label = toupper(country_name)), stat = "unique", hjust = 0, size = 50, alpha = 0.1, family = f1b),
  # country line from min year to max year
  geom_segment(aes(x = min(start_year - y_off), y = 0,
                   xend = max(start_year - y_off), yend = 0), stat = "unique", size = 5, lineend = "round", color = "#1139A0"),
  # "branches" 
  geom_segment(aes(x = start_year - y_off, y = 0,
                   xend = start_year, yend = y_alt * y_off, 
                   color = city), stat = "unique", size = 5, lineend = "round", color = "#1139A0"),
  geom_segment(aes(x = start_year, y = y_alt * y_off,
                   xend = end_year, yend = y_alt * y_off,
                   color = city), stat = "unique", size = 4),
  # scales, coord, theme
  scale_x_continuous(minor_breaks = 2005:2030, breaks = seq(2005, 2030, by = 5), limits = c(2004, 2030)),
  ylim(-3.5, 3.5),
  # coord_fixed(clip = "off"),
  theme_minimal(base_family = f1b, base_size = 16),
  theme(
    plot.background = element_rect(fill = "#e7e8ee", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom",
    # legend.spacing.x = unit(1.5, 'lines'),
    legend.text = element_text(hjust = 0, margin = margin(0, 20, 0, 0)),
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(family = f1, color = "grey60"),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor.x = element_line(color = "grey87"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = f1b, size = 34, margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey50", family = f2, margin = margin(30, 0, 0, 0))
  )
)

# Greece plot
p_gr <- ggplot(transit_gr) +
  gg_common +
  # start and end points
  geom_point(data = subset(transit_gr, start_year <= 2020),
             aes(x = start_year, y = y_alt * y_off, color = city), shape = 21, fill = "white", size = 4, stroke = 4) +
geom_point(data = subset(transit_gr, end_year <= 2020),
           aes(x = end_year, y = y_alt * y_off, color = city), shape = 21, fill = "white", size = 4, stroke = 4) +
  # tunneled
  geom_segment(aes(x = start_year, y = y_alt * y_off,
                   xend = start_year + tunnel_per * (end_year - start_year), yend = y_alt * y_off), stat = "unique", size = 1, lineend = "round", color = "white") +
  # annotations
  annotate("text", 2021.5, 1.8, label = "Main line (€1.1 bn)", hjust = 0, family = f2, size = 4, color = "grey20") +
  annotate("text", 2021, -0.9, label = "Kalamaria Extension (€0.6 bn)", hjust = 0, family = f2, size = 4, color = "grey20") +
  annotate("text", 2021.5, 0.9, label = "Line 3 to Piraeus (€0.7 bn)", hjust = 0, family = f2, size = 4, color = "grey20") +
  annotate("text", 2027.5, -1.8, label = "Line 4 (€1.3 bn)", hjust = 0, family = f2, size = 4, color = "grey20") +
  scale_color_manual(values = c("#80BB56", "#AB40A7")) +
  # Title
  labs(title = "Transit projects in Greece and Sweden")
  
# Sweden plot
p_se <- ggplot(transit_se) +
  gg_common +
  # start and end points
  geom_point(data = subset(transit_se, start_year <= 2020), aes(start_year, y_alt * y_off, color = city), shape = 21, fill = "white", size = 4, stroke = 4) +
geom_point(data = subset(transit_se, end_year <= 2020), aes(end_year, y_alt * y_off, color = city), shape = 21, fill = "white", size = 4, stroke = 4) +
  # tunneled
  geom_segment(aes(x = start_year, y = y_alt * y_off, xend = start_year + tunnel_per * (end_year - start_year), yend =  y_alt * y_off), stat = "unique", size = 1, lineend = "round", color = "white") +
  # annotations
  annotate("text", 2011, -0.9, label = "Citytunneln (SEK 8.45 bn)", hjust = 0, family = f2, size = 4) +
  annotate("segment", x = 2006.75, y = -0.9, xend = 2006.75, yend = -1.9, color = "white") +
  annotate("text", 2006.75, -2.3, label = "35% of the line\nis tunneled", family = f1, color = "grey40", vjust = 1, lineheight = 0.9) +
  annotate("text", 2008, 1.5, label = "Citybanan (SEK 16.8 bn)", hjust = 0, family = f2, size = 4) +
  annotate("segment", x = 2015.1, y = 0.9, xend = 2015.1, yend = 1.9, color = "white") +
  annotate("text", 2015.1, 2.3, label = "81% is tunneled", family = f1, color = "grey40") +
  annotate("text", 2030, 2.4, label = "Arenastaden (SEK 4.7 bn)", hjust = 1, family = f2, size = 4) +
  annotate("text", 2030, -1.2, label = "Nacka/Söderort (SEK 14.4 bn)", hjust = 1, family = f2, size = 4) +
  annotate("text", 2026, -3.3, label = "Barkarby (SEK 3.3 bn)", hjust = 1, family = f2, size = 4) +
  scale_color_manual(values = c("#DC473A", "#F5CD47")) +
  # caption
  labs(caption = "Source: Transit Costs Project | Graphic: Georgios Karamanis")

# Cowplot
plot_grid(p_gr, p_se, ncol = 1, rel_heights = c(1, 0.926)) 

ggsave(here::here("temp", paste0("transit-cost-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9.34, width = 10)

