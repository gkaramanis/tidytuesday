library(tidyverse)
library(ggtext)
library(here)

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

counties <- map_data("county")

measles_counties <- measles %>% 
  filter(lng < 0) %>% 
  filter(mmr > 0) %>% 
  mutate(county = tolower(county)) %>% 
  group_by(county) %>% 
  summarise(county_mean = mean(mmr, na.rm = TRUE)) %>%
  ungroup() %>% 
  full_join(counties, by = c("county" = "subregion")) %>% 
  mutate(
    mmr_color = case_when(
      county_mean > 89.999 ~ "#275D8E",
      is.na(county_mean) ~ "#F7EBD3",
      TRUE ~ "#DB3A2F"
    )
  )

ggplot(measles_counties) +
  geom_polygon(aes(long, lat, group = group, fill = mmr_color), color = "#0B0C0B", size = 0.1) +
  annotate("text", -124, 28, label = "*Data for more than 46,000 schools in 704\ncounties in 32 states. 119 counties (16.9%)\nhad a mean vaccination rate lower than 90%\nand 119 counties had 90% or higher.", hjust = 0, family = "IBM Plex Sans") +
  annotate("text", -124, 25, label = "Source: The Wall Street Journal | Graphic: Georgios Karamanis", hjust = 0, size = 2.5, family = "IBM Plex Sans Light") +
  coord_map() +
  scale_fill_identity() +
  labs(title = "1 in 6 counties* have a mean vaccination rate for MMR <span style = 'color:#DB3A2F;'>below 90%</span>") +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    plot.title = element_markdown(size = 20, hjust = 0.5, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
    ) +
  ggsave(here::here("2020-week09", "plots", "measles.png"), dpi = 320, width = 12, height = 8)
