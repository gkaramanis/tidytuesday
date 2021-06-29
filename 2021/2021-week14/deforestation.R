library(tidyverse)
library(fuzzyjoin)
library(janitor)

brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')

# total area lost by year
brz <- brazil_loss %>% 
  pivot_longer(cols = c(-1:-3)) %>% 
  group_by(year) %>% 
  summarise(total_area = sum(value) / 100) # convert to km2

# sum total_area
brz_sum <- data.frame(total_area = sum(brz$total_area))

# country surface areas
areas <- read_csv(here::here("2021", "2021-week14", "data", "country_profile_variables.csv")) %>% 
  clean_names() %>% 
  select(country, total_area = surface_area_km2) %>% 
  mutate(
    total_area = as.numeric(total_area),
    total_area = na_if(total_area, -99)
    ) %>% 
  filter(!is.na(total_area)) %>% 
  filter(country != "Vanuatu") # remove island country for legibility

# icon mappings
mapping <- read_csv(here::here("2020-week52", "data", "mapglyphs-mapping.csv"))

# match area to countries
brz_areas <- brz %>% 
  distance_left_join(areas, max_dist = 1300) %>% 
  left_join(mapping) %>% 
  filter(!is.na(icon)) %>% 
  group_by(year) %>% 
  filter(row_number() == max(row_number()))

brz_sum_area <- brz_sum %>% 
  distance_left_join(areas, max_dist = 7000) %>% 
  left_join(mapping)

# fonts
f1 = "MapGlyphs"
f2 = "Publico Headline"
f2b = "Publico Headline Bold"
f3b = "DIN Condensed Bold"

# colors
bg_col <- "grey97"
brown_col <- "#B49F89"
green_col <- "#009c3b"
yellow_col <- "#ffdf00"
blue_col <- "#002776"

# Plot
ggplot(brz_areas) +
  # Brazil icon
  annotate("text", x = -10000, y = 2012.5, label = subset(mapping, country == "Brazil")$icon, family = f1, size = 140, color = "grey85") +
  # bars
  geom_bar(aes(x = total_area.x, y = year), stat = "identity", orientation = "y", width = 0.8, fill = brown_col) +
  # year
  geom_text(aes(x = 0, y = year, label = if_else((year - 1) %% 4 == 0, year, NULL)), family = f3b, size = 9, hjust = 1, nudge_x = -1000, color = brown_col) +
  # country icon
  geom_text(aes(x = total_area.x, y = year, label = icon), family = f1, size = 9, nudge_y = -0.35, nudge_x = 5000, hjust = 1, color = green_col) +
  # country label
  geom_text(aes(x = total_area.x, y = year, label = country), family = f3b, lineheight = 0.8, hjust = 0, size = 4, nudge_x = 5500, color = "grey25") +
  # area label
  geom_text(aes(x = total_area.x, y = year, label = format(total_area.x, big.mark = " ")), family = f3b, lineheight = 0.8, hjust = 1, size = 5.5, nudge_x = -1000, color = bg_col) +
  # title and text
  annotate("text", x = -25000, y = 2002, label = "Deforestation\nin Brazil", size = 16, family = f3b, hjust = 0.5, color = "grey20", lineheight = 0.7) +
  annotate("text", x = -25000, y = 2004,
           label = str_wrap(paste0("From 2001 to 2013, Brazil lost a total of ", format(brz_sum_area$total_area.x, big.mark = " "), " km² of forest area, or roughly the size of ", brz_sum_area$country), 22),
           size = 7, family = f2, hjust = 0.5, vjust = 1, color = "grey20", lineheight = 1) +
  annotate("text", x = -24000, y = 2011.5, label = brz_sum_area$icon, family = f1, size = 60, color = green_col) +
  annotate("text", x = -27000, y = 2009, label = brz_sum_area$country, family = f3b, size = 4, color = "grey25") +
  annotate("text", x = -25000, y = 2011,
           label = str_wrap("The graphic shows the forest area lost by year and the country with a similar area (not in scale)", 35),
           size = 4, family = f2, hjust = 0.5, vjust = 1, color = "grey20", lineheight = 1) +
  annotate("text", x = -25000, y = 2013,
           label = "Source: Our World in Data · Graphic: Georgios Karamanis",
           color = "grey40", family = f2, size = 2.5, hjust = 0.5
           ) +
  scale_y_reverse() +
  coord_cartesian(clip = "off", expand = FALSE) +
  xlim(-42000, 54000) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = bg_col, color = NA)
  ) 

ggsave(here::here("temp", paste0("deforestation-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 6)

