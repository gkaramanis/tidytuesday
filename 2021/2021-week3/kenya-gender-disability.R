library(tidyverse)
library(rKenyaCensus)
library(janitor)
library(cowplot)
library(sf)
library(ggtext)

# Read in data for plot 1 ----------------------------------------------------
gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv') %>% 
  clean_names() %>% 
  filter(county == "Total") %>% 
  rename_with(~ paste0("pop_", .), -matches("county"))

disability <- V4_T2.27 %>% 
  clean_names() %>% 
  filter(sub_county == "KENYA") %>% 
  mutate(county = "Total") %>% 
  select(-sub_county, -admin_area) %>% 
  pivot_longer(contains(c("male", "female", "total")),
               names_to = c("disability", ".value"),
               names_pattern = "(.+)_(.+)",
               values_to = "total") %>% 
  ungroup() %>% 
  mutate(not_mf = total - male - female)


# Calculate ratios (country totals) -------------------------------------------
disability_ratios <- disability %>% 
  left_join(gender, by = "county") %>% 
  mutate(
    ratio_male = round(male / pop_male * 100, 2),
    ratio_female = round(female / pop_female * 100, 2),
    ratio_not_mf = round(not_mf / pop_intersex * 100, 2)
  ) %>% 
  select(disability, starts_with("ratio")) %>% 
  pivot_longer(starts_with("ratio"))


# Read in data for plot 2 -------------------------------------------------
centroids <- CountyGPS %>% 
  clean_names() %>% 
  mutate(county = tools::toTitleCase(tolower(county)))

shp <- read_sf(here::here("2021", "2021-week3", "data", "gadm36_KEN_0.shp")) %>% 
  st_simplify(preserveTopology = F, dTolerance = 0.1) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  select(x, y)

disability_counties <- V4_T2.27 %>% 
  clean_names() %>% 
  filter(admin_area == "County") %>% 
  select(-sub_county, -admin_area) %>% 
  pivot_longer(contains(c("male", "female", "total")),
               names_to = c("disability", ".value"),
               names_pattern = "(.+)_(.+)",
               values_to = "total") %>% 
  ungroup() %>% 
  mutate(
    county = tools::toTitleCase(tolower(county)),
    not_mf = total - male - female
  )

gender_counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv') %>% 
  clean_names() %>% 
  filter(county != "Total") %>% 
  rename_with(~ paste0("pop_", .), -matches("county"))


# Calculate ratios by county ----------------------------------------------
disability_ratios_counties <- disability_counties %>% 
  left_join(gender_counties, by = "county") %>% 
  mutate(
    ratio_male = round(male / pop_male * 100, 2),
    ratio_female = round(female / pop_female * 100, 2),
    ratio_not_mf = round(not_mf / pop_intersex * 100, 2)
  ) %>% 
  select(county, disability, starts_with("ratio")) %>% 
  pivot_longer(starts_with("ratio")) %>% 
  left_join(centroids) %>% 
  mutate(
    name = fct_relevel(name, c("ratio_male", "ratio_female", "ratio_not_mf"))
  )

# Capitalize labels in plots ----------------------------------------------
cap_label <- function(x) {tools::toTitleCase(str_replace(x, "_", " "))}

# Plot 1 ------------------------------------------------------------------
p1 <- ggplot(disability_ratios) +
  geom_path(aes(value, disability, group = disability), color = "grey50", size = 0.75) +
  geom_point(aes(value, disability, color = name), size = 16) +
  geom_text(aes(value, disability, label = value), size = 4, color = "white", check_overlap = TRUE, family = "Produkt Medium") +
  scale_x_continuous(limits = c(0, 3), minor_breaks = seq(0, 3, by = 0.1), breaks = seq(0, 3, by = 0.5)) +
  scale_y_discrete(label = cap_label, limits = rev(levels(factor(disability_ratios$disability)))) +
  scale_color_manual(values = c("#E06843", "#4A9EA8", "#6E1AA3")) +
  labs(
    title = "<span>Disability prevalence in <span style = 'color:#4A9EA8;'>men</span>, <span style = 'color:#E06843;'>women</span> and <span style = 'color:#6E1AA3;'>intersex</span>* people of Kenya</span>",
    subtitle = "*Inferred prevalence, as disabilities in intersex people are not explicitily reported",
    caption = "Source: rKenyaCensus | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Futura Medium") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FDF1E6", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", size = 0.3),
    panel.grid.minor.x = element_line(color = "grey80", size = 0.15),
    plot.title = element_markdown(hjust = 0, size = 24, margin = margin(10, 0, 0, 0), family = "Produkt Medium"),
    plot.subtitle = element_text(hjust = 0, size = 18, margin = margin(10, 0, 30, 0), family = "Produkt"),
    plot.caption = element_text(hjust = 0, size = 12, margin = margin(30, 0, 0, 0), family = "Produkt", color = "grey30")
  ) 

# Plot 2 ------------------------------------------------------------------

p2 <- ggplot(disability_ratios_counties) +
  geom_polygon(data = shp, aes(x, y), color = NA, fill = "grey85") +
  geom_point(aes(longitude, latitude, color = name, size = value), alpha = 0.9) +
  scale_size_continuous(range = c(0, 5)) +
  scale_color_manual(values = c("#4A9EA8", "#E06843", "#6E1AA3"), guide = FALSE) +
  coord_fixed(clip = "off") +
  facet_grid(disability ~ name, labeller = labeller(disability = cap_label)) +
  theme_void(base_family = "Futura Medium") +
  theme(
    legend.position = "none",
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 12, hjust = 0, color = "grey30", margin = margin(0, 10, 0, 10)),
    plot.background = element_rect(fill = "#FDF1E6", color = NA),
    plot.margin = margin(20, 30, 30, 20)
  ) 


# Combine plots -----------------------------------------------------------
set_null_device("png")

ggdraw(p1) +
  draw_plot(p2, scale = 0.785, x = 0.33, y = -0.03) 

ggsave(here::here("temp", paste0("kenya-gender-disability", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 10)

