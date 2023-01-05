library(tidyverse)
library(eurostat)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 8, height = 9, units = "in", dpi = 320)

# Get spatial data for map
nuts2 <- eurostat_geodata_60_2016 %>% 
  janitor::clean_names() %>% 
  filter(levl_code == 2)

# Search for datasets
srch <- search_eurostat("death") %>% 
  distinct()

# Get causes of death dataset
cause <- get_eurostat("hlth_cd_ysdr2")

top_cause <- cause %>% 
  # Keep only latest year
  filter(time == "2017-01-01") %>% 
  # Keep only total deaths
  filter(sex == "T") %>% 
  filter(age == "TOTAL") %>%
  # Keep specific causes
  filter(icd10 != "A-R_V-Y") %>% 
  # Group by region and keep the top cause
  group_by(geo) %>% 
  slice_max(order_by = values, n = 1, with_ties = FALSE) %>% 
  ungroup()

# Join geodata and top causes
nuts2_cause <- nuts2 %>% 
  left_join(top_cause) 

# Get world coastline, to be used as "background"
world_coast <- sf::read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson")

# Fonts
f1 <- "Outfit"

# Plot
ggplot(nuts2_cause) +
  geom_sf(data = world_coast, linewidth = 0.15, color = "white") +
  geom_sf(aes(fill = icd10), linewidth = 0.15, color = "white") +
  scale_fill_manual(values = c("#5B84B1", "#FC766A"), breaks = c("C00-D48", "I"),  labels = c("Cancer¹", "Cardiovascular diseases²")) +
  coord_sf(xlim = c(-22.5, 44), ylim = c(30, 70.5)) +
  labs(
    title = "Leading causes of death",
    subtitle = "Standardised death rate by NUTS 2 region of residence, 3 year average",
    caption = "¹Neoplasms (ICD-10 codes C00-D48); ²Diseases of the circulatory system (ICD-10 codes I00-I99)\nSource: Eurostat · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), lineheight = 1)
  )
    