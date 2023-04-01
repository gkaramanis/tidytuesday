library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')

tz_now <- transitions %>% 
  mutate(end = str_replace(end, "32767", "3767")) %>% 
  mutate(
    td = today(),
    begin_date = as_date(begin),
    end_date = as_date(end)
  ) %>% 
  filter(between(td, begin_date, end_date)) %>% 
  mutate(
    dst = ifelse((year(begin_date) != 2023 & year(end_date) != 2023), "no change", dst)
  )

tz_sf <- read_sf(here::here("2023/2023-week_13/data/tz.geojson")) %>% 
  rmapshaper::ms_simplify()

f1 <- "Outfit"
f2 <- "Rowan"

tz_plot <- tz_sf %>% 
  st_make_valid() %>% 
  janitor::clean_names() %>% 
  left_join(tz_now, by = c("tzid" = "zone")) %>% 
  mutate(dst = fct_relevel(dst, c("TRUE", "no change", "FALSE")))

ggplot(tz_plot) +
  geom_sf(aes(fill = dst), color = "white", linewidth = 0.1) +
  scale_fill_manual(values = c("orange2", "darkgreen", "purple2"),
                    labels = c("Yes, it is", "Not using DST", "No, it is not"),
                    name = NULL,
                    drop = TRUE) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  guides(fill = guide_legend(title.position = "top", ncol = 2, byrow = TRUE)) +
  labs(
    title = "Is it Daylight Saving Time (DST) today*?",
    caption = glue::glue("*Today's date is {today()} · Source: IANA tz database · Graphic: Georgios Karamanis")
    ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12, margin = margin(0, 15, 0, 0)),
    legend.spacing.y = unit(0.2, "line"),
    legend.margin = margin(0, 0, 20, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, family = f2),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(0, 10, 0, 10)
  ) 

  
