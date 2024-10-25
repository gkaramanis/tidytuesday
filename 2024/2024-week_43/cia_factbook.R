library(tidyverse)
library(countrycode)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

# Using 2024 data, downloaded from https://www.cia.gov/the-world-factbook/
cia_exp <- read_csv(here::here("2024/2024-week_43/data/Life expectancy at birth.csv")) %>% 
  select(name, life_exp_at_birth = years, region)

cia_pop <- read_csv(here::here("2024/2024-week_43/data/Population - total.csv")) %>% 
  select(name, population = value, region)

cia_factbook <- cia_exp %>% 
  left_join(cia_pop)

cia_fb <- cia_factbook %>% 
  mutate(
    continent = case_when(
      str_detect(region, "America") ~ "Americas",
      str_detect(region, "Asia") ~ "Asia",
      str_detect(region, "East") ~ "Asia",
      str_detect(region, "Oceania") ~ "Oceania",
      TRUE ~ region
    ),
    r = sqrt(population / pi) / 0.8e5,
    ) %>% 
  mutate(
    c = as.numeric(as.factor(continent)),
    y = c - r
    ) %>% 
  group_by(continent) %>% 
  mutate(
    highlight = case_when(
      population > 200e6 ~ TRUE,
      life_exp_at_birth > 85 ~ TRUE,
      life_exp_at_birth < 58 ~ TRUE,
      life_exp_at_birth == min(life_exp_at_birth) ~ TRUE,
      life_exp_at_birth == max(life_exp_at_birth) ~ TRUE,
      TRUE ~ FALSE
    ),
    med_life_exp = median(life_exp_at_birth)
  ) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(cia_fb, aes(x = life_exp_at_birth, y = y)) +
  # Continents
  geom_segment(aes(x = 49.5, xend = 90.5, y = c), stat = "unique", color = "darkgreen", alpha = 0.2) +
  geom_text(aes(x = 49.5, y = c, label = continent), stat = "unique", hjust = 0, vjust = 0, size = 24, alpha = 1, family = f2, fontface = "bold", color = "#EFE9D9") +
  # Countries
  geom_point(aes(size = population), shape = 21, fill = "white", alpha = 0.6) +
  ggrepel::geom_label_repel(data = . %>% filter(highlight), aes(x = life_exp_at_birth, y = y - r, label = paste(name, round(life_exp_at_birth, 1))), direction = "y", nudge_y = 0.12, family = f1b, min.segment.length = 0, segment.size = 0.2, label.size = 0, label.padding = 0.2, size = 3.5) +
  # Median life expectancy
  geom_text(aes(x = med_life_exp, y = c + 0.03, label = paste0("▲", "\n", round(med_life_exp, 1))), stat = "unique", vjust = 1, color = "#EA8B70", size = 3.5, lineheight = 0.9, fontface = "bold") +
  scale_x_continuous(limits = c(49, 91)) +
  scale_y_reverse() +
  scale_size_area(max_size = 25) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Global demographics: life expectancy and population",
    subtitle = str_wrap("Countries plotted by life expectancy at birth and continent, with population represented by circle size. Median life expectancy per continent shown with triangles. Labels highlight most populous nations and those with highest/lowest life expectancies in each continent.", 128),
    caption = "Source: The World Factbook 2024 · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 20, family = f2),
    plot.subtitle = element_text(lineheight = 1, margin = margin(3, 0, 10, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
  
