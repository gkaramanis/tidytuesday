library(tidyverse)
library(camcorder)
library(patchwork)
library(ggtext)

gg_record(dir = "tidytuesday-temp", device = "png", width = 6, height = 9, units = "in", dpi = 320)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

# Read in paygap data
paygap_pc <- paygap %>% 
  mutate(
    postcode = str_remove_all(post_code, " |\\.|,"),
    pc_area = str_sub(postcode, 1, 2)
  ) %>%
  group_by(pc_area, employer_size) %>% 
  summarize(
    hourly_med = median(diff_median_hourly_percent, na.rm = TRUE),
    bonus_med = median(diff_median_bonus_percent, na.rm = TRUE),
    employer_size,
    n = n()
  ) %>% 
  ungroup() %>% 
  distinct()

# Run this part to create grid from postcode_polygons.gpkg
# library(geogrid)
# polygons <- sf::read_sf(here::here("2022/2022-week_26/data/postcode_polygons.gpkg")) %>% 
#   rmapshaper::ms_simplify()
# polygons_grid <- calculate_grid(shape = polygons, grid_type = "hex", seed = 3)
# gb_grid <- assign_polygons(polygons, polygons_grid)
# sf::st_write(gb_grid, here::here("2022/2022-week_26/data/gb-grid/gb-grid.shp"))

# Read in grid shapefile
gb_grid <- sf::read_sf(here::here("2022/2022-week_26/data/gb-grid/gb-grid.shp"))

# Combine data with grid
paygap_area <- gb_grid %>% 
  left_join(paygap_pc) %>% 
  filter(employer_size != "Not Provided" & !is.na(employer_size)) %>% 
  mutate(
    employer_size = if_else(employer_size == "Less than 250", "Less than 250 employees", employer_size),
    employer_size = fct_relevel(employer_size,
                                     c("Less than 250 employees",
                                       "250 to 499",
                                       "500 to 999",
                                       "1000 to 4999",
                                       "5000 to 19,999",
                                       "20,000 or more"))
  )

# Fonts
f1 <- "Familjen Grotesk"

# Main plot
p1 <- ggplot(paygap_area) +
  geom_sf(data = gb_grid, fill = "grey20", size = 0.1) +
  geom_sf(aes(fill = hourly_med), size = 0.1, color = "gray0") +
  scale_fill_fermenter(type = "div",  palette = "PRGn", direction = 1, limits = c(-40, 40), breaks = seq(-40, 40, 10), labels = abs, guide = guide_colorsteps(title = "Median % difference in hourly pay\nby postcode area and company size", title.position = "top", show.limits = TRUE)) +
  annotate("segment", x = 1.9e5, xend = 5.4e5, y = Inf, yend = Inf, size = 0.25, color = "grey20") +
  facet_wrap(vars(employer_size)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2.5, "line"),
    legend.key.height = unit(0.4, "line"),
    legend.title.align = 0.5,
    legend.margin = margin(0, 0, 10, 0),
    legend.title = element_text(margin = margin(0, 0, 5, 0)),
    plot.background = element_rect(fill = "gray97", color = NA),
    strip.text = element_text(margin = margin(0, 0, 4, 0), color = "grey30"),
    plot.margin = margin(0, 0, 75, 0)
  )

# Distribution plot
p2 <- paygap_pc %>% 
  ggplot() +
  geom_density(aes(abs(hourly_med), fill = hourly_med > 0), alpha = 0.6, color = NA) +
  geom_richtext(aes(x = 25, y = 0.08, label = "Distribution of median % difference in hourly pay<br> for <span style = 'color:#7B3294;'>women</span> and <span style = 'color:#008837;'>men</span>"), color = "grey30", hjust = 0, family = f1, size = 3, stat = "unique", fill = NA, label.color = NA) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(name = "PRGn", n = 5)[c(1, 5)], labels = c("Women", "Men")) +
  scale_x_continuous(breaks =  c(seq(0, 60, 10)), labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(breaks = c(0.2, 0.4)) +
  coord_cartesian(expand = FALSE) +
  labs(
    caption = "Source: Gender pay gap service · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 6, color = "grey60", margin = margin(3, 0, 0, 0)),
    axis.text.y = element_text(size = 6, color = "grey60", margin = margin(0, 3, 0,)),
    plot.background = element_rect(fill = NA, color = NA),
    plot.caption = element_text(size = 7, margin = margin(10, 0, 0, 0), color = "grey20", hjust = 0)
  )

# Annotation plot
a <- ggplot() +
  geom_text(aes(x = c(-0.55, 0.55), y = 0, label = c("Women earn more", "Men earn more"), color = c("women", "men")), hjust = c(1, 0), family = f1, size = 3) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(name = "PRGn", n = 5)[c(5, 1)]) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none")

# Combine plots
p1 +
  inset_element(p2, 0.01, -0.17, 0.95, 0.05) +
  inset_element(a, 0, 1, 1, 1.17) +
  plot_annotation(theme = theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )
  )

                    