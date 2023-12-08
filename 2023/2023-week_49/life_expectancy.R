library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

life_expectancy_different_ages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_different_ages.csv') %>%
  janitor::clean_names()

f1 <- "Outfit"

le_change <- life_expectancy_different_ages %>% 
  group_by(entity) %>% 
  filter(year == 2021 | year == 1961) %>% 
  filter(!is.na(code)) %>% 
  pivot_wider(names_from = year, values_from = c(life_expectancy0, life_expectancy10, life_expectancy25, life_expectancy45, life_expectancy65, life_expectancy80)) %>% 
  summarise(
    code,
    `Life expectancy at age 0` = `life_expectancy0_2021`-`life_expectancy0_1961`,
    `At age 10` = `life_expectancy10_2021`-`life_expectancy10_1961`,
    `At age 25` = `life_expectancy25_2021`-`life_expectancy25_1961`,
    `At age 45` = `life_expectancy45_2021`-`life_expectancy45_1961`,
    `At age 65` = `life_expectancy65_2021`-`life_expectancy65_1961`,
    `At age 80` = `life_expectancy80_2021`-`life_expectancy80_1961`
  ) %>% 
  pivot_longer(`Life expectancy at age 0`:`At age 80`, names_to = "age", values_to = "change") %>% 
  mutate(age = fct_inorder(age))

world <- giscoR::gisco_get_countries() %>% 
  janitor::clean_names() %>% 
  rename(code = iso3_code) %>% 
  st_transform(crs = "+proj=moll")

world_le_2021 <- world %>% 
  left_join(le_change) %>% 
  filter(!is.na(change))

ggplot(world_le_2021) +
  geom_sf(aes(fill = change), linewidth = 0.05, color = "black") +
  colorspace::scale_fill_binned_diverging("Purple-Brown", rev = TRUE) +
  facet_wrap(vars(age), ncol = 2) +
  guides(fill = guide_colorsteps(title.position = "top", show.limits = TRUE)) +
  labs(
    title = "Change in life expectancy\nbetween 1961 and 2021",
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis",
    fill = "Years"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    panel.grid = element_line(linewidth = 0.1, color = "grey70"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )

