library(tidyverse)
library(fuzzyjoin)
library(ggbump)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

# Read in plant image, assign numbers to parts
plant <- read_csv("2020-week34/images/plant.csv") %>% 
  filter(c != "#FFFFFF") %>% 
  mutate(
    y = 24 - y,
    n = row_number(),
    l = case_when(
      n %in% c(1:3, 4:8, 11:16, 21:26) ~ 1,
      n %in% c(9:10, 17:20, 27:30, 31:34, 37) ~ 2,
      n %in% c(35:36, 38:42, 46:48) ~ 3,
      n %in% c(43:45, 49:52, 53:56, 60, 66) ~ 4,
      n %in% c(57:59, 61:65, 70:75, 81:83, 84) ~ 5,
      n %in% c(67:69, 76:80, 85:90, 91:97) ~ 6),
    r = case_when(
      n %in% c(98, 102:103, 106, 112, 117, 123, 129, 135) ~ 1,
      n %in% c(101, 100, 105, 111, 115, 121, 127, 133) ~ 2,
      n %in% c(116, 122, 128, 134) ~ 3,
      n %in% c(107:109, 113, 118, 125, 131, 137) ~ 4,
      n %in% c(99, 104, 110, 114, 119:120, 126, 132) ~ 5,
      n %in% c(124, 130, 136) ~ 6
      )
    ) 


# Sum threats and actions -------------------------------------------------
euro_plants <- plants %>% 
  pivot_longer(cols = starts_with("threat"), names_to = "threat", values_to = "threat_value") %>%
  group_by(binomial_name) %>% 
  mutate(sum_threats = sum(threat_value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = threat, values_from = threat_value) %>% 
  pivot_longer(cols = starts_with("action"), names_to = "action", values_to = "action_value") %>% 
  group_by(binomial_name) %>% 
  mutate(sum_actions = sum(action_value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = action, values_from = action_value) %>% 
  select(binomial_name, country, continent, sum_threats, sum_actions) %>% 
# Filter Europe -----------------------------------------------------------
  filter(continent == "Europe") %>% 
  arrange(country) %>% 
# Coordinates for the layout of plants ------------------------------------
  mutate(
    row = 35 * c(3, 2, 2, 1, 1, 1, 1, 1, 0, -1, -2, -2, -3),
    col = 15 * c(0, -1, 1, -4, -2, 0, 2, 4, 0, 0, -1, 1, 0),
    line_x1 = col + 10,
    line_y1 = row + 30,
    line_x2 = c(seq(-80, 120, length.out = 5), c(rep(-100, 3), rep(-20, 3), 60, 140)),
    line_y2 = c(rep(160, 5), rep(-130, 8))
    ) %>% 

# Total species by country  -----------------------------------------------
  group_by(country) %>% 
  mutate(species_n = row_number() - 1) %>%  # -1 for easier labelling later
  ungroup()

# Join leaves, keep parts according to number of threats ------------------
euro_plants_leaves <- euro_plants %>% 
  fuzzy_left_join(plant, by = c("sum_threats" = "l"), match_fun = list(`<`)) %>% 
  mutate(
    x = x + col,
    y = y + row
    )

# Join roots, keep parts according to number of actions ------------------
euro_plants_roots <- euro_plants %>% 
  fuzzy_left_join(plant, by = c("sum_actions" = "r"), match_fun = list(`>=`)) %>% 
  mutate(
    x = x + col,
    y = y + row
    )

# Make data frame to draw many plants according to the layout -------------
plant_layout <- plant %>% 
  mutate(
    row = list(euro_plants$row),
    col = list(euro_plants$col)
    ) %>% 
  unnest(c(row, col)) %>% 
  mutate(
    x = x + col,
    y = y + row
  )

# Plot --------------------------------------------------------------------
ggplot(euro_plants) +
# Country circles and names -----------------------------------------------
  geom_point(aes(x = line_x2 - nchar(str_replace_all(country, " ", "")) * 4.6, y = line_y2 + 4, size = species_n), color = "pink") +
  geom_text(aes(x = line_x2, y = line_y2, label = country), stat = "unique", size = 3, hjust = 1, nudge_x = -10, family = "PT Mono Bold") +
# Curves ------------------------------------------------------------------
  geom_sigmoid(aes(x = line_x1, y = line_y1,  xend = line_x2 + species_n * 8, yend = line_y2 * 0.98, group = binomial_name), direction = "y", linetype = "dotted", color = "#92AA82", smooth = 5) +
  geom_point(aes(x = line_x1, y = line_y1), color = "#92AA82") +
  geom_segment(aes(x = line_x2 - 7, y = line_y2, xend = line_x2 + species_n * 8 + 7, yend = line_y2), size = 0.25) +
# Species names -----------------------------------------------------------
  geom_text(aes(x = line_x2 + species_n * 8, y = line_y2 * 1.02, label = binomial_name, hjust = if_else(line_y2 > 0, 0, 1)), size = 2.3, angle = 45, family = "Produkt") +
# Plants ------------------------------------------------------------------
  geom_tile(data = plant_layout, aes(x, y), fill = "grey85") +
  geom_tile(data = euro_plants_leaves, aes(x, y, fill = c)) +
  geom_tile(data = euro_plants_roots, aes(x, y, fill = c)) +
# Annotations -------------------------------------------------------------
  annotate("segment", x = -100, y = 55, xend = -65, yend = 55, color = "#92AA82", linetype = "dashed") +
  annotate("text", x = -110, y = 59, label = "Missing leaves = \nnumber of threats", hjust = 1, lineheight = 0.9, family = "Produkt", size = 3, color = "#008F51") +
  annotate("segment", x = -100, y = 40, xend = -65, yend = 40, color = "#92AA82", linetype = "dashed") +
  annotate("text", x = -110, y = 38, label = "Number of roots =\nnumber of actions", hjust = 1, lineheight = 0.9, family = "Produkt", size = 3, color = "#945200") +
  annotate("segment", x = -137, y = -82, xend = -137, yend = -115, color = "#92AA82", linetype = "dashed") +
  annotate("text", x = -137, y = -70, label = "Extinct species\nper country", lineheight = 0.9, family = "Produkt", size = 3, color = "#F29DB0") +
# Title -------------------------------------------------------------------
  labs(title = expression(underline("EUROPE"))) +
# Caption -----------------------------------------------------------------
  annotate("text", x = 190, y = 0, label = "Source: IUCN | Graphic: Georgios Karamanis", angle = 90, size = 2.5, family = "Produkt", color = "grey50") +
# Scales, theme, etc. -----------------------------------------------------
  scale_fill_identity() +
  scale_size_continuous(range = c(4, 8)) +
  coord_fixed(clip = "off", expand = FALSE) +
  xlim(-190, 190) +
  ylim(-140, 180) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(50, 20, 70, 20),
    plot.background = element_rect(fill = "#F2F5F7", color = NA),
    plot.title = element_text(size = 18, family = "Produkt", hjust = 0.52, margin = margin(0, 0, 60, 0))
  ) 

ggsave(here::here("temp", paste0("plants-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 8, height = 9)

