library(tidyverse)
library(waffle)
library(ggflags)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 9.5, units = "in", dpi = 320)

# Read in data
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')

nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')

# Count birth countries
birth_countries <- nhl_rosters %>% 
  filter(season == 20232024) %>% 
  count(birth_country, sort = TRUE, name = "m") %>% 
  mutate(birth_country = fct_reorder(birth_country, m))

# 2023-2024 season, join teams and birth countries
nhl_bc <- nhl_rosters %>% 
  filter(season == 20232024) %>% 
  count(team_code, birth_country) %>% 
  left_join(nhl_teams) %>% 
  left_join(birth_countries) %>% 
  mutate(
    flag_code = tolower(countrycode::countrycode(birth_country, origin = "iso3c", destination = "iso2c"))
    )

# Fonts
f1 <- "Radio Canada Condensed"
f2 <- "Radio Canada"

# Waffle plot
p <- ggplot(nhl_bc) +
  geom_pictogram(aes(label = flag_code, values = n), n_rows = 5, flip = TRUE) +
  facet_wrap(vars(full_name)) 

# Teams with row numbers
team_numbers <- nhl_bc %>% 
  distinct(full_name) %>% 
  arrange(full_name) %>% 
  mutate(team_i = as.factor(row_number()))

# Get the plot data and join team numbers
pb <- ggplot_build(p) %>% 
  .$data %>%
  .[[1]] %>% 
  rename(panel = PANEL) %>% 
  left_join(team_numbers, by = c("panel" = "team_i"))

# Final plot
ggplot(pb) +
  # Outline for flags
  geom_point(aes(x, y), size = 7, color = "#010101") +
  # flags
  geom_flag(aes(x, y, country = label), size = 5.4) +
  scale_country() +
  facet_wrap(vars(str_wrap(full_name, 12)), ncol = 7, strip.position = "bottom") +
  coord_fixed(ratio = 0.95, clip = "off", ylim = c(0.6, 6.2)) +
  labs(
    title = "Birth countries of NHL players in the 2023-2024 season",
    caption = "Source: NHL · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing = unit(1, "lines"),
    panel.spacing.x = unit(1.5, "lines"),
    strip.text = element_text(size = 11, color = "#010101", face = "bold", margin = margin(2, 0, 2, 0), lineheight = 0.85),
    strip.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 20, hjust = 0.5, margin = margin(5, 0, 10, 0), family = f2, face = "bold"),
    plot.caption = element_text(size = 11)
  )
