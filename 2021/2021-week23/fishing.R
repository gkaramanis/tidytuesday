library(tidyverse)
library(ggfx)
library(ggimage)
library(ragg)
library(wesanderson)
library(ggtext)
library(sf)
library(patchwork)

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

top_sp <- fishing %>% 
  filter(region == "U.S. Total") %>% 
  filter(year > 1914) %>% 
  group_by(lake, species) %>% 
  summarise(total = round(sum(values, na.rm = TRUE))) %>% 
  slice_max(n = 3, order_by = total) %>% 
  mutate(
    i = row_number(),
    lake = paste0("Lake ", lake)
    ) %>% 
  ungroup()

top_fishing <- fishing %>% 
  filter(region == "U.S. Total") %>% 
  filter(year > 1914) %>% 
  mutate(lake = paste0("Lake ", lake)) %>% 
  inner_join(top_sp) %>% 
  mutate(
    species_img = if_else(species == "Cisco and chubs", "chubs", tolower(species)),
    img = paste0(here::here("2021", "2021-week23", "data", "img"), "/", species_img, ".png"),
    lbl = paste0("**", i, ". ", species, "**", "<br>",
                 format(total, big.mark = " "), " lbs.", if_else(species == "Blue Pike", "<br>(Total production 1915-2015)", ""))
    )

# Lakes shapefiles
erie <- st_read("2021/2021-week23/data/shp/hydro_p_LakeErie/hydro_p_LakeErie.shp")
michigan <- st_read("2021/2021-week23/data/shp/hydro_p_LakeMichigan/hydro_p_LakeMichigan.shp")
superior <- st_read("2021/2021-week23/data/shp/hydro_p_LakeSuperior/hydro_p_LakeSuperior.shp")
lakes <- st_read("2021/2021-week23/data/shp/greatlakes/GL210429_lam.shp")


# Plot --------------------------------------------------------------------

f1 = "Charter"
f2 = "Fira Sans Condensed"
pal <- wes_palette("Zissou1", 10, "continuous")

agg_png(here::here("temp", paste0("fishing-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 320, height = 12, width = 9, units = "in")

# Fish plot
p <- ggplot(top_fishing) +
  geom_image(aes(x = 1965, y = 1, image = img), stat = "unique", size = 0.9, asp = 1) +
  as_reference(
    geom_image(aes(x = 1965, y = 1, image = img), stat = "unique", size = 0.9, asp = 1),
    id = "fish"
  ) +
  with_blend(
    geom_tile(aes(x = year, y = 0, width = 1, height = 11, fill = values)),
    bg_layer = "fish",
    blend_type = "in"
  ) +
  geom_richtext(aes(x = 1965, y = -2, label = lbl), stat = "unique", family = f1, fill = NA, label.color = NA, vjust = 1) +
  scale_fill_gradientn(colors = pal, labels=function(x) format(x, big.mark = " "), name = "lbs.") +
  facet_grid(rows = vars(i), cols = vars(lake)) +
  labs(
    title = "Commercial fish production in the Great Lakes 1915-2015",
    subtitle = "U.S. production by year for the top 3 species in Lakes Erie, Michigan and Superior",
    caption = "Source: Great Lakes Fishery Commission | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#F5F4EF", color = NA),
    legend.position = c(0.35, 1.1),
    legend.direction = "horizontal",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(0.5, "line"),
    axis.text.x = element_text(margin = margin(10, 0, 0, 0)),
    strip.text = element_text(family = f1, size = 14),
    strip.text.x = element_text(size = 16, margin = margin(80, 0, 10, 0), face = "bold"),
    strip.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.2, color = "grey80"),
    plot.margin = margin(20, 20, 20, 10),
    plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = f2),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "grey30", margin = margin(20, 0, 0, 0))
    )

# Lakes plot
l <- ggplot() +
  geom_sf(data = lakes, fill = "grey80", color = NA) +
  geom_sf(data = subset(erie, COUNTRY == "USA"), fill = "lightblue2", color = NA) +
  geom_sf(data = michigan, fill = "lightblue2", color = NA) +
  geom_sf(data = subset(superior, COUNTRY == "USA"), fill = "lightblue2", color = NA) +
  geom_sf(data = lakes, size = 0.1, fill = NA, color = "grey50") +
  geom_sf_text(data = subset(erie, COUNTRY == "USA"), aes(label = NAMEEN), family = f1, size = 2.25, fontface = "bold") +
  geom_sf_text(data = michigan, aes(label = NAMEEN), family = f1, size = 2.25, fontface = "bold") +
  geom_sf_text(data = subset(superior, COUNTRY == "USA"), aes(label = NAMEEN), family = f1, size = 2.25, fontface = "bold") +
  theme_void() 

# Annotation
a <- ggplot() +
  geom_path(aes(x = c(0, 0, 3, 3), y = c(0, 0.2, 0.2, 0)), color = "grey30", size = 0.4) +
  geom_text(aes(x = 1.5, y = 0.75, label = "The Alewife invasion"), family = f1, size = 3.5) +
  coord_fixed(clip = "off") +
  theme_void()

p + inset_element(l, 0.62, 0.82, 0.77, 0.92, align_to = "full") +
  inset_element(a, 0.45, 0.7, 0.54, 0.8, align_to = "full") +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "#F5F4EF", color = NA)))

dev.off()

