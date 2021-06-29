library(tidyverse)
library(janitor)
library(sf)
library(colorspace)

broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv') %>% 
  clean_names() %>% 
  mutate(
    across(4:5, as.numeric),
    county_name = str_remove(county_name, " County| Parish")
    ) %>% 
  rename(state = st)
  
usa <- read_sf(here::here("2021", "2021-week19", "data", "c_10nv20", "c_10nv20.shp")) %>% 
  clean_names() %>% 
  rename(county_name = countyname) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>% 
  left_join(broadband, by = c("state", "county_name")) %>% 
  mutate(diff = 100 * (broadband_usage - broadband_availability_per_fcc)) %>% 
  filter(!is.na(diff)) # filter out NA values

f1 = "Charter Roman"
f1bb = "Charter Black"
f2 = "Fira Sans"
f2b = "Fira Sans Bold"
f3 = "Fira Sans Compressed"

pal <- diverge_hcl(4, rev = TRUE)

ggplot(usa) +
  geom_sf(aes(geometry = geometry, fill = diff), color = "grey97", size = 0.1) +
  annotate("text", x = -65, y = 56, label = toupper("Disparities in assessment of broadband access"), family = f2b, size = 8, lineheight = 0.9, hjust = 1) +
  annotate("text", x = -65, y = 54, label = "Difference between FCC and Microsoft data in percentage of households connected at broadband speeds, defined as at least\n25 Mbps download speed. Microsoft data show a lower broadband access in 98% of all counties compared to the FCC data.", family = f1, size = 3.75, hjust = 1, vjust = 1) +
  annotate("text", x = -123, y = 25, label = "FCC data from the end of 2017. Microsoft data from November 2019\nGraphic: Georgios Karamanis", family = f2, size = 2) +
  scale_fill_stepsn(colors = pal, breaks = c(-50, 0, 50),
guide = guide_colorsteps(title.position = "top"), "Difference in percentage points") +
  coord_sf() +
  theme_void() +
  theme(
    legend.position = c(0.2, 0.125),
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "line"),
    legend.text = element_text(family = f3),
    legend.title = element_text(family = f2, size = 6, margin = margin(0, 0, 3, 0)),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(10, 0, 10, 0)
  ) 

ggsave(here::here("temp", paste0("broadband-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 6)
