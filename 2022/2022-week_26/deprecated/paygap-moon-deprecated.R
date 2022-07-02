library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

polygons <- sf::read_sf(here::here("2022/2022-week_26/data/postcode_polygons.gpkg")) %>% 
  rmapshaper::ms_simplify()

gb <- polygons %>% 
  sf::st_union()

paygap_pc <- paygap %>% 
  mutate(
    postcode = str_remove_all(post_code, " |\\.|,"),
    pc_area = str_sub(postcode, 1, 2)
  ) %>% 
  group_by(pc_area, employer_size) %>% 
  summarize(
    hourly_med = median(diff_median_hourly_percent, na.rm = TRUE),
    bonus_med = median(diff_median_bonus_percent, na.rm = TRUE),
    employer_size,
    n = n()
  ) %>% 
  ungroup() %>% 
  distinct()


paygap_area <- polygons %>% 
  left_join(paygap_pc) %>% 
  filter(employer_size != "Not Provided" & !is.na(employer_size)) %>% 
  mutate(employer_size = fct_relevel(employer_size,
                                     c("Less than 250",
                                       "250 to 499",
                                       "500 to 999",
                                       "1000 to 4999",
                                       "5000 to 19,999",
                                       "20,000 or more"))
  )

ggplot(paygap_area) +
  geom_sf(data = gb, fill = "gray40", color = NA) +
  geom_sf(aes(fill = hourly_med), size = 0.1, color = "gray0") +
  scale_fill_fermenter(type = "div",  palette = "PRGn", direction = 1, limits = c(-40, 40), breaks = seq(-40, 40, 10)) +
  facet_wrap(vars(employer_size)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray17", color = NA)
  )
