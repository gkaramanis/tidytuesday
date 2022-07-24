library(tidyverse)
library(geomtextpath)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 16, height = 10, units = "in", dpi = 320)

technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

nordics <- c("ALA", "DNK", "FIN", "FRO", "GRL", "ISL", "NOR", "SWE")

pop <- read_csv(here::here("2022/2022-week_29/data/WPP2022_Demographic_Indicators_Medium.csv")) %>% 
  janitor::clean_names() %>% 
  filter(iso3_code %in% nordics) %>% 
  select(year = time, iso3c = iso3_code, name = location, pop = t_population1july) # population is in thousands

beds <- technology %>% 
  filter(str_detect(label, "Beds")) %>% 
  filter(iso3c %in% nordics) %>% 
  left_join(pop) %>% 
  mutate(beds_pop = value / pop)

pal <- c("#913238", "#3C6698", "#C4343A", "#091F57", "#D1AD38") # DNK, FIN, ISL, NOR, SWE

f1 <- "Outfit"
f2 <- "Fira Sans Condensed"

theme_set(theme_minimal(base_family = f1))

theme_update(
  legend.position = "none",
  plot.background = element_rect(fill = "#fbf7f5", color = NA),
  axis.title = element_blank(),
  strip.text = element_text(size = 12, color = "grey15", hjust = 0, vjust = 0)
)

p1 <- ggplot(beds %>% filter(label == "Beds in hospitals")) +
  geom_textline(aes(x = year, y = beds_pop, group = iso3c, label = iso3c, color = iso3c), size = 3, vjust = -0.25, family = f1, linewidth = 0.75, hjust = 0.6) +
  scale_color_manual(values = pal) +
  scale_y_continuous(breaks = seq(2, 16, 2), limits = c(2, 18)) +
  facet_wrap(vars(label)) +
  theme(
    strip.text = element_text(size = 14)
  )

p2 <- ggplot(beds %>% filter(label != "Beds in hospitals")) +
  geom_textline(aes(x = year, y = beds_pop, group = iso3c, label = iso3c, color = iso3c), size = 3, vjust = -0.25, family = f1, linewidth = 0.75, hjust = 0.75) +
  scale_color_manual(values = pal) +
  facet_wrap(vars(label), scales = "free", labeller = labeller(label = label_wrap_gen(38))) 

m <- geofacet::europe_countries_grid2 %>% 
  ggplot(aes(col, -row)) +
  geom_tile(aes(width = 0.95, height = 0.9, fill = if_else(name %in% beds$name, name, "x"))) +
  geom_text(aes(label = code), size = 2.5, color = "white", family = f2, fontface = "bold") +
  scale_fill_manual(values = c(pal, "#CEBEB5")) +
  theme_void() +
  theme(legend.position = "none")

p1  +
  inset_element(m, 0.55, 0.87, 0.98, 1) +
  p2 +
  plot_annotation(
    title = "Beds in health facilities in the Nordics",
    subtitle = "Per 1 000 people, varying time periods",
    caption = "Source: data.nber.org & UN · Graphic: Georgios Karamanis",
    theme = theme(
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(margin = margin(10, 0, 0, 0)),
      plot.margin = margin(20, 20, 10, 20)
    )
  )

