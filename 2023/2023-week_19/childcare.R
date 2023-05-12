library(tidyverse)
library(sf)
library(cetcolor)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')

counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

cc <- childcare_costs %>% 
  select(county_fips_code, study_year, mc_toddler, mhi_2018) %>% 
  mutate(cc_pc_inc = mc_toddler / mhi_2018 * 100) %>% 
  left_join(counties) %>% 
  mutate(
    county_fips_code = str_pad(county_fips_code, 5, pad = 0),
    id = as.character(county_fips_code)
    )

us <- read_sf("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json") %>% 
  janitor::clean_names() %>% 
  rmapshaper::ms_simplify(keep = 0.1)

us <- urbnmapr::get_urbn_map("counties", sf = TRUE) %>% 
  rename(county_fips_code = county_fips) %>% 
  rmapshaper::ms_simplify(keep = 0.1)

us_cc <- us %>% 
  left_join(cc, by = "county_fips_code")

f1 <- "Outfit"

ggplot(us_cc %>% filter(study_year > 2009)) +
  geom_sf(aes(fill = cc_pc_inc, color = cc_pc_inc), linewidth = 0.05) +
  coord_sf(xlim = c(-2500000, 2500000), ylim = c(-2300000,  730000)) +
  scale_color_gradientn(colours = cet_pal(5, name = "l18"), na.value = "grey60", guide = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "l18"), na.value = "grey60", guide = guide_colorbar(title.position = "top"), labels = scales::percent) +
  facet_wrap(vars(study_year)) +
  labs(
    title = "Childcare Prices as a Share of Median Family Income, 2010-2018",
    subtitle = "For toddler center-based childcare. Median household income in 2018 dollars.",
    caption = "Source: National Database of Childcare Prices · Graphic: Georgios Karamanis",
    fill = "Share of median family income"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(2, "lines"),
    strip.text = element_text(margin = margin(5, 0, 0, 0)),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(10, 0, 5, 0)
  )
  